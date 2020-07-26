##### Libraries #####

library(dplyr)
library(tidyr)
library(readr)
library(pracma)
library(rstanarm)
library(loo)
library(Metrics)
library(mgcv)
library(earth)
library(ggplot2)


##### CSV loading #####
data <- read.csv("MexicoCovid19Updated.csv", header = T, sep = ",")
data$Date <- as.Date(data$Date,"%Y-%m-%d")
population = read.csv("./population.csv",header=T)
names = c("BAJA CALIFORNIA","CHIHUAHUA","COAHUILA","DURANGO","NUEVO LEÃ“N",
          "SINALOA","SONORA","TAMAULIPAS","ZACATECAS")

for (i in names){
  data$population[data$Region==i] <- population[population$Estado==i,2]
}

# A random 120 year old person appears, gratz mate!
max(data$Age)

##### Preliminary plotting #####
# hist sex
plot(data$Sex)

# hist age 0-20 20-30 30-40 40-50 50-60 60-70 70-80 80+
data$age_class <- factor(data$Age%/%10)
plot(data$age_class)

# cumulative
data$time <- data$Date-data$Date[1]
plot(data$time,rownames(data),type="l")

# by regions
plot(data$Region, las=3)

# Daily data
data_daily <- data %>%
  group_by(time) %>%
  summarize(daily_cases = n())


# Adding lockdown and post_lockdown columns
first_day_data <- min(data$Date)
lockdown_start <- as.Date("2020-03-23")
lockdown_end <- as.Date("2020-06-01")
last_day_data <- max(data$Date)

no_lock <- rep(0,as.integer(lockdown_start - first_day_data))
yes_lock <- seq(1,as.integer(lockdown_end - lockdown_start))
no_lock_again <- rep(0,as.integer(last_day_data - lockdown_end)+1)
lockdown <- c(no_lock, yes_lock, no_lock_again)
data_daily$lockdown <- lockdown

no_lock <- rep(0,as.integer(lockdown_end - first_day_data))
post_lock <- seq(1,as.integer(last_day_data - lockdown_end)+1)
post_lockdown <- c(no_lock, post_lock)
data_daily$post_lockdown <- post_lockdown

# Daily data show a strong periodicity due to (possibly) sunday testing?
plot(data_daily$time, data_daily$daily_cases, type = "l")

# Moving averages make them smooth
data_daily$avg_cases <- movavg(data_daily$daily_cases, n= 7, type="s")
data_daily$avg_cases <- as.integer(data_daily$avg_cases)
data_daily$time <- as.integer(data_daily$time)
plot(data_daily$time, data_daily$avg_cases, type = "l")


lag1 <- c(NA, data_daily$avg_cases[1:199])
lag2 <- c(NA, NA, data_daily$avg_cases[1:198])
data_daily$lag1 <- lag1 
data_daily$lag2 <- lag2

# cumulative data
data_daily$cumulative = cumsum(data_daily$daily_cases)

##### Train/Test split #####
test_days = 5
days <- as.integer(length(data_daily$time))
training_set <- data_daily[data_daily$time<days-test_days,]
test_set <- data_daily[data_daily$time>=days-test_days,]

##### Plot and Loss functions #####

# bayesian
loss_stan <- function(model, test, data_daily){

  pred_model <- posterior_predict(model, newdata = test)
  train_model <- posterior_predict(model)
  pred_0025 <- apply(pred_model, 2, function(x) quantile(x, 0.025))
  pred_0975 <- apply(pred_model, 2, function(x) quantile(x, 0.975))
  pred_mean <- apply(pred_model, 2, mean)
  train_mean <- apply(train_model, 2, mean)
  train_0025 <- apply(train_model, 2, function(x) quantile(x, 0.025))
  train_0975 <- apply(train_model, 2, function(x) quantile(x, 0.975))
  
  train_df <- data.frame(cbind(time = data_daily$time[1:195],
                               train_mean, train_0025, train_0975))
  pred_df <- data.frame(cbind(time = data_daily$time[196:200],
                              pred_mean, pred_0025, pred_0975))
  plot <- ggplot() +
    geom_point(data = data_daily, aes(x=time, y=avg_cases)) +
    geom_line(data = pred_df, 
              aes(x=time,y=pred_mean),
              col="orange",
              lwd=0.8) +
    geom_ribbon(data = pred_df, 
                aes(x=time,ymin=pred_0025, ymax=pred_0975), alpha=0.2,
                fill = "orange") +
    geom_line(data = train_df,
              aes(x=time, y=train_mean),
              col="blue",
              lwd=0.8) +
    geom_ribbon(data = train_df,
                aes(x=time, ymin=train_0025, ymax=train_0975), alpha=0.2,
                fill = "blue") +
    theme_classic() +
    ggtitle(model$call)  
  
  print(plot)
  return(mse(test$avg_cases, pred_mean))
}

# bayesian + updating lag
loss_stan_lag <- function(model, test, data_daily){
  df <- test
  pred_model <- posterior_predict(model, newdata = df[1,])
  pred <- apply(pred_model, 2, mean)
  pred_low <- apply(pred_model, 2, function(x) quantile(x, 0.025))
  pred_high <- apply(pred_model, 2, function(x) quantile(x, 0.975))
  ms <- mse(as.double(df[1,"avg_cases"]), pred)
  
  pred_mean <- pred
  pred_0025 <- pred_low
  pred_0975 <- pred_high

  df[1, "avg_cases"] <- pred_mean

  #If we are just predicting one day then we are done
  if (dim(df)[1] == 1){
    return(ms)
  }
  
  #Yes, I know having not one but two nested loops is bad in R
  #But look, there will not be that many iterations, we are dealing
  #With short predictions here
  for (i in 2:dim(df)[1]){
    df[i, "lag1"] <- df[i-1, "avg_cases"]
    #Need to update each lag column
    for (k in 2:i){
      col <- paste("lag",k,sep="")
      col_prev <- paste("lag",k-1, sep="")
      df[i, col] <- df[i-1, col_prev]
    }
    pred_model <- posterior_predict(model, newdata = df[i,])
    
    pred <- apply(pred_model, 2, mean)
    pred_low <- apply(pred_model, 2, function(x) quantile(x, 0.025))
    pred_high <- apply(pred_model, 2, function(x) quantile(x, 0.975))
    
    pred_mean <- c(pred_mean, pred)
    pred_0025 <- c(pred_0025, pred_low)
    pred_0975 <- c(pred_0975, pred_high)
    
    ms <- ms + mse(as.double(df[i,"avg_cases"]), pred)
    df[i, "avg_cases"] <- pred

  }
  
  train_model <- posterior_predict(model)
  train_mean <- apply(train_model, 2, mean)
  train_0025 <- apply(train_model, 2, function(x) quantile(x, 0.025))
  train_0975 <- apply(train_model, 2, function(x) quantile(x, 0.975))
  
  train_df <- data.frame(cbind(time = data_daily$time[3:195],
                               train_mean, train_0025, train_0975))
  pred_df <- data.frame(cbind(time = data_daily$time[196:200],
                              pred_mean, pred_0025, pred_0975))
  plot <- ggplot() +
    geom_point(data = data_daily, aes(x=time, y=avg_cases)) +
    geom_line(data = pred_df, 
              aes(x=time,y=pred_mean),
              col="orange",
              lwd=0.8) +
    geom_ribbon(data = pred_df, 
                aes(x=time,ymin=pred_0025, ymax=pred_0975), alpha=0.2,
                fill = "orange") +
    geom_line(data = train_df,
              aes(x=time, y=train_mean),
              col="blue",
              lwd=0.8) +
    geom_ribbon(data = train_df,
                aes(x=time, ymin=train_0025, ymax=train_0975), alpha=0.2,
                fill = "blue") +
    theme_classic() +
    ggtitle(model$call)  
  
  print(plot)

  return(ms/length(pred_mean))
}


# frequentist
loss <- function(model, test, data_daily){
  pred <- predict(model, newdata = test, type = "response", se.fit = TRUE)
  pred_mean <- pred$fit
  pred_0025 <- pred$fit - 2*pred$se.fit
  pred_0975 <- pred$fit + 2*pred$se.fit
  
  train_model <- predict(model, type="response", se.fit=TRUE)
  train_mean <- train_model$fit
  train_0025 <- train_model$fit - 2*train_model$se.fit
  train_0975 <- train_model$fit + 2*train_model$se.fit
  train_df <- data.frame(cbind(time = data_daily$time[1:195],
                               train_mean, train_0025, train_0975))
  pred_df <- data.frame(cbind(time = data_daily$time[196:200],
                              pred_mean, pred_0025, pred_0975))
  plot <- ggplot() +
    geom_point(data = data_daily, aes(x=time, y=avg_cases)) +
    geom_line(data = pred_df, 
              aes(x=time,y=pred_mean),
              col="orange",
              lwd=0.8) +
    geom_ribbon(data = pred_df, 
                aes(x=time,ymin=pred_0025, ymax=pred_0975), alpha=0.2,
                fill = "orange") +
    geom_line(data = train_df,
              aes(x=time, y=train_mean),
              col="blue",
              lwd=0.8) +
    geom_ribbon(data = train_df,
                aes(x=time, ymin=train_0025, ymax=train_0975), alpha=0.2,
                fill = "blue") +
    theme_classic() +
    ggtitle(model$call)    
  
  print(plot)
  return(mse(test$avg_cases, pred_mean))
}


loss_lag <- function(model, test, data_daily){
  df <- test
  predictions <- predict(model, newdata = df[1,], type="response", se.fit = TRUE,
                         interval = "pint")
  pred_model <- predictions$fit
  ms <- mse(as.double(df[1,"avg_cases"]), pred_model)
  pred_mean <- pred_model
  pred_0025 <- pred_mean - 2*predictions$se.fit
  pred_0975 <- pred_mean + 2*predictions$se.fit
  
  df[1, "avg_cases"] <- pred_model
  
  #If we are just predicting one day then we are done
  if (dim(df)[1] == 1){
    return(ms)
  }
  
  #Yes, I know having not one but two nested loops is bad in R
  #But look, there will not be that many iterations, we are dealing
  #With short predictions here
  for (i in 2:dim(df)[1]){
    df[i, "lag1"] <- df[i-1, "avg_cases"]
    #Need to update each lag column
    for (k in 2:i){
      col <- paste("lag",k,sep="")
      col_prev <- paste("lag",k-1, sep="")
      df[i, col] <- df[i-1, col_prev]
    }
    
    predictions <- predict(model, newdata = df[i,], type="response", se.fit = TRUE,
                           interval = "se")
    pred_model <- predictions$fit
    pred_mean <- c(pred_mean, pred_model)
    pred_0025 <- c(pred_0025, pred_model - 2*predictions$se.fit)
    pred_0975 <- c(pred_0975, pred_model + 2*predictions$se.fit)
    
    ms <- ms + mse(as.double(df[i,"avg_cases"]), pred_model)
    df[i, "avg_cases"] <- pred_model
    
  }
  
  train_model <- predict(model, type="response", se.fit=TRUE)
  train_mean <- train_model$fit
  train_0025 <- train_model$fit - 2*train_model$se.fit
  train_0975 <- train_model$fit + 2*train_model$se.fit

  train_df <- data.frame(cbind(time = data_daily$time[3:195],
                               train_mean, train_0025, train_0975))
  pred_df <- data.frame(cbind(time = data_daily$time[196:200],
                              pred_mean, pred_0025, pred_0975))
  plot <- ggplot() +
    geom_point(data = data_daily, aes(x=time, y=avg_cases)) +
    geom_line(data = pred_df, 
              aes(x=time,y=pred_mean),
              col="orange",
              lwd=0.8) +
    geom_ribbon(data = pred_df, 
                aes(x=time,ymin=pred_0025, ymax=pred_0975), alpha=0.2,
                fill = "orange") +
    geom_line(data = train_df,
              aes(x=time, y=train_mean),
              col="blue",
              lwd=0.8) +
    geom_ribbon(data = train_df,
                aes(x=time, ymin=train_0025, ymax=train_0975), alpha=0.2,
                fill = "blue") +
    theme_classic() +
    ggtitle(model$call)    
  
  print(plot)

  return(ms/length(pred_mean)) 
}

loss_earth <- function(model, test){
  df <- test
  predictions <- predict(model, newdata = df[1,], type="response", interval = "pint")
  pred_model <- predictions$fit
  ms <- mse(as.double(df[1,"avg_cases"]), pred_model)
  pred_mean <- pred_model
  pred_0025 <- predictions$lwr
  pred_0975 <- predictions$upr
  
  df[1, "avg_cases"] <- pred_model
  
  #If we are just predicting one day then we are done
  if (dim(df)[1] == 1){
    return(ms)
  }
  
  #Yes, I know having not one but two nested loops is bad in R
  #But look, there will not be that many iterations, we are dealing
  #With short predictions here
  for (i in 2:dim(df)[1]){
    df[i, "lag1"] <- df[i-1, "avg_cases"]
    #Need to update each lag column
    for (k in 2:i){
      col <- paste("lag",k,sep="")
      col_prev <- paste("lag",k-1, sep="")
      df[i, col] <- df[i-1, col_prev]
    }
    
    predictions <- predict(model, newdata = df[i,], type="response", interval = "pint")
    pred_model <- predictions$fit
    pred_mean <- c(pred_mean, pred_model)
    pred_0025 <- c(pred_0025, predictions$lwr)
    pred_0975 <- c(pred_0975, predictions$upr)
    
    ms <- ms + mse(as.double(df[i,"avg_cases"]), pred_model)
    df[i, "avg_cases"] <- pred_model
    
  }
  
  train_model <- predict(model, type="response", interval= "pint")
  train_mean <- train_model$fit
  train_0025 <- train_model$lwr
  train_0975 <- train_model$upr
  
  train_df <- data.frame(cbind(time = data_daily$time[3:195],
                               train_mean, train_0025, train_0975))
  pred_df <- data.frame(cbind(time = data_daily$time[196:200],
                              pred_mean, pred_0025, pred_0975))
  plot <- ggplot() +
    geom_point(data = data_daily, aes(x=time, y=avg_cases)) +
    geom_line(data = pred_df, 
              aes(x=time,y=pred_mean),
              col="orange",
              lwd=0.8) +
    geom_ribbon(data = pred_df, 
                aes(x=time,ymin=pred_0025, ymax=pred_0975), alpha=0.2,
                fill = "orange") +
    geom_line(data = train_df,
              aes(x=time, y=train_mean),
              col="blue",
              lwd=0.8) +
    geom_ribbon(data = train_df,
                aes(x=time, ymin=train_0025, ymax=train_0975), alpha=0.2,
                fill = "blue") +
    theme_classic() +
    ggtitle(model$call)    
  
  print(plot)
  
  return(ms/length(pred_mean)) 
}


plot(data_daily$time, data_daily$avg_cases, type = "l")
abline(v=c(82, 152), lty=2)

##### Poisson Regression #####

# time: this is just hopeless
model_time <- stan_glm(avg_cases ~ time + lag2, family = poisson,  data=training_set)
loss_stan_lag(model_time, test_set, data_daily)

# time^2
model_time_2_no_lag <- stan_glm( avg_cases ~ time + I(time^2), family = poisson,  data=training_set)
loss_stan(model_time_2_no_lag, test_set, data_daily) #116642

model_time_2_lag2 <- stan_glm( avg_cases ~ time + I(time^2) + lag2, family = poisson,  data=training_set)
loss_stan_lag(model_time_2_lag2, test_set, data_daily) #32918

# time^3
model_time_3_no_lag <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=training_set)
loss_stan(model_time_3_no_lag, test_set, data_daily) #159659 -> Super high if no lag is used

model_time_3_lag_2 <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3) + lag2, family = poisson,  data=training_set)
loss_stan_lag(model_time_3, test_set, data_daily) #14714

model_time_3_lag_1_2 <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3) + lag2 + lag1, family = poisson,  data=training_set)
loss_stan_lag(model_time_3_lag_1_2, test_set, data_daily) #34599 


# exp(time) !!!!!!
model_time_exp <- glm(avg_cases ~ time + I(time^2) + I(exp(time)), family = poisson,  data=training_set)
loss(model_time_exp, test_set)

# lockdown
model_time_lock <- stan_glm(avg_cases ~ time +  I(time^2) + post_lockdown + lockdown, family = poisson,  data=training_set )
loss_stan(model_time_lock, test_set, data_daily)

# lockdown with lag and updated predictions
model_time_lock_lag2 <- stan_glm(avg_cases ~ time +  I(time^2) + lag2 + post_lockdown + lockdown, family = poisson,  data=training_set )
loss_stan_lag(model_time_lock_lag2, test_set, data_daily) #27924

model_time_lock_3_lag_2 <- stan_glm(avg_cases ~ time +  I(time^2) + I(time^3) + lag2 + post_lockdown + lockdown, family = poisson,  data=training_set )
loss_stan_lag(model_time_lock_3_lag_2, test_set, data_daily) #12182

##### GAM ######

#boh boh
model_gam <- gam(avg_cases ~  s(time) + lag2, method="REML", family = poisson(), data = training_set)
loss(model_gam, test_set) #25846
loss_lag(model_gam, test_set) #95120: peggiora notevolmente, non capisco bene perché


##### MARS #####
?earth
training_set_3 <- training_set[3:dim(training_set)[1],]
mars <- earth(avg_cases ~ time + lag2, data = training_set_3, varmod.method = "power",
              nfold = 5, ncross = 30)
loss_earth(mars, test_set)

################################################################################
###################### DIVISION ################################################
################################################################################

# SEX

female <- data %>%
  group_by(Sex, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Sex=="FEMININO")

male  <- data %>%
  group_by(Sex, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Sex=="MASCULINO")

plot(female$time, female$daily_cases, type = "l")
points(male$time, male$daily_cases, type="l", col=2)

# moving avg
female$avg_cases <- movavg(female$daily_cases, n= 7, type="s")
male$avg_cases <- movavg(male$daily_cases, n= 7, type="s")
plot(female$time, female$avg_cases, type = "l")
points(male$time, male$avg_cases, type="l", col=2)

# REGIONS

##### Regions utils ######

fill_region <- function(region){
  region <- region %>%
    complete(time = seq(0,199,1)) 
  region[is.na(region)] <- 0
  return(region)
}

prepare_region <- function(region){
  region$avg_cases <- as.integer(region$avg_cases)
  region$time <- as.integer(region$time)
  lag1 <- c(NA, region$avg_cases[1:199])
  lag2 <- c(NA, NA, region$avg_cases[1:198])
  region$lag1 <- lag1 
  region$lag2 <- lag2
  region_training <- region[region$time<days-test_days,]
  region_test <- region[region$time>=days-test_days,]
  return(list(region_training,region_test))
}

BajaCalifornia <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="BAJA CALIFORNIA")

Chihuahua <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="CHIHUAHUA")
Chihuahua <- fill_region(Chihuahua)

Coahuila <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="COAHUILA")

Durango <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="DURANGO")

NuevoLeon <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="NUEVO LEON")

NuevoLeon <- fill_region(NuevoLeon)

Sinaloa <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="SINALOA")

Sonora <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="SONORA")

Tamaulipas <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="TAMAULIPAS")
Tamaulipas <- fill_region(Tamaulipas)

Zacatecas <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="ZACATECAS")

# plots
plot(BajaCalifornia$time, BajaCalifornia$daily_cases, xlab="time", ylab="cases", type = "l", ylim = c(0,1000))
points(Chihuahua$time, Chihuahua$daily_cases, type="l", col=2)
points(Coahuila$time, Coahuila$daily_cases, type="l", col=3)
points(Durango$time, Durango$daily_cases, type="l", col=4)
points(NuevoLeon$time, NuevoLeon$daily_cases, type="l", col=5)
points(Sinaloa$time, Sinaloa$daily_cases, type="l", col=6)
points(Sonora$time, Sonora$daily_cases, type="l", col=7)
points(Tamaulipas$time, Tamaulipas$daily_cases, type="l", col=8)
points(Zacatecas$time, Zacatecas$daily_cases, type="l", col=9)

# moving avg
BajaCalifornia$avg_cases <- movavg(BajaCalifornia$daily_cases, n= 7, type="s")
Chihuahua$avg_cases <- movavg(Chihuahua$daily_cases, n= 7, type="s")
Coahuila$avg_cases <- movavg(Coahuila$daily_cases, n= 7, type="s")
Durango$avg_cases <- movavg(Durango$daily_cases, n= 7, type="s")
NuevoLeon$avg_cases <- movavg(NuevoLeon$daily_cases, n= 7, type="s")
Sinaloa$avg_cases <- movavg(Sinaloa$daily_cases, n= 7, type="s")
Sonora$avg_cases <- movavg(Sonora$daily_cases, n= 7, type="s")
Tamaulipas$avg_cases <- movavg(Tamaulipas$daily_cases, n= 7, type="s")
Zacatecas$avg_cases <- movavg(Zacatecas$daily_cases, n= 7, type="s")

# smooth plots
plot(BajaCalifornia$time, BajaCalifornia$avg_cases, xlab="time", ylab="cases", type = "l", ylim = c(0,800))
points(Chihuahua$time, Chihuahua$avg_cases, type="l", col=2)
points(Coahuila$time, Coahuila$avg_cases, type="l", col=3)
points(Durango$time, Durango$avg_cases, type="l", col=4)
points(NuevoLeon$time, NuevoLeon$avg_cases, type="l", col=5)
points(Sinaloa$time, Sinaloa$avg_cases, type="l", col=6)
points(Sonora$time, Sonora$avg_cases, type="l", col=7)
points(Tamaulipas$time, Tamaulipas$avg_cases, type="l", col=8)
points(Zacatecas$time, Zacatecas$avg_cases, type="l", col=9)


##################################################
####### Models for the three biggest regions #####
##################################################

###### Nuevo Leon ######
NuevoLeon_training <- prepare_region(NuevoLeon)[[1]]
NuevoLeon_test <- prepare_region(NuevoLeon)[[2]]

model_time_3_no_lag_leon <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=NuevoLeon_training)
loss_stan(model_time_3_no_lag_leon, NuevoLeon_test, NuevoLeon) #2160

model_time_3_lag_leon <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3) +lag2, family = poisson,  data=NuevoLeon_training)
loss_stan_lag(model_time_3_lag_leon, NuevoLeon_test, NuevoLeon) #267353 -> Interesting, it has an almost exp growth

model_gam_no_lag_leon <- gam(avg_cases ~  s(time), method="REML", family = poisson(), data = NuevoLeon_training)
loss(model_gam_no_lag_leon, NuevoLeon_test, NuevoLeon) #12139

model_gam_lag_leon <- gam(avg_cases ~  s(time) + lag2, method="REML", family = poisson(), data = NuevoLeon_training)
loss_lag(model_gam_lag_leon, NuevoLeon_test, NuevoLeon) #61690: also this one "runs away"

##### Chihuahua ######
Chihuahua_training <- prepare_region(Chihuahua)[[1]]
Chihuahua_test <- prepare_region(Chihuahua)[[2]]


model_time_3_no_lag_chi <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=Chihuahua_training)
loss_stan(model_time_3_no_lag_chi, Chihuahua_test, Chihuahua) #12803

model_time_3_lag_chi <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3) +lag2, family = poisson,  data=Chihuahua_training)
loss_stan_lag(model_time_3_lag_chi, Chihuahua_test, Chihuahua) #15713 -> This time it decreases too much!

model_gam_no_lag_chi <- gam(avg_cases ~  s(time), method="REML", family = poisson(), data = Chihuahua_training)
loss(model_gam_no_lag_chi, Chihuahua_test, Chihuahua) #5068

model_gam_lag_chi <- gam(avg_cases ~  s(time) + lag2, method="REML", family = poisson(), data = Chihuahua_training)
loss_lag(model_gam_lag_chi, Chihuahua_test, Chihuahua) #1741: hmmm

##### Tamaulipas ######

Tamaulipas_training <- prepare_region(Tamaulipas)[[1]]
Tamaulipas_test <- prepare_region(Tamaulipas)[[2]]

model_time_3_no_lag_tama <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=Tamaulipas_training)
loss_stan(model_time_3_no_lag_tama, Tamaulipas_test, Tamaulipas) #25276

model_time_3_lag_tama <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3) +lag2, family = poisson,  data=Tamaulipas_training)
loss_stan_lag(model_time_3_lag_tama, Tamaulipas_test, Tamaulipas) #1171 -> This one is sooo good

model_gam_no_lag_tama <- gam(avg_cases ~  s(time), method="REML", family = poisson(), data = Tamaulipas_training)
loss(model_gam_no_lag_tama, Tamaulipas_test, Tamaulipas) #24222

model_gam_lag_tama <- gam(avg_cases ~  s(time) + lag2, method="REML", family = poisson(), data = Tamaulipas_training)
loss_lag(model_gam_lag_tama, Tamaulipas_test, Tamaulipas) #2903: ok, but still not good as before. Splines are officialy ko


# AGE

young <- data %>%
  group_by(age_class, time) %>%
  summarize(daily_cases = n()) %>%
  filter(age_class %in% c(0,1,2))%>%
  group_by(time) %>%
  summarize(daily_cases = sum(daily_cases))

middle_age <- data %>%
  group_by(age_class, time) %>%
  summarize(daily_cases = n()) %>%
  filter(age_class %in% c(3,4,5)) %>%
  group_by(time) %>%
  summarize(daily_cases = sum(daily_cases))

old <-data %>%
  group_by(age_class, time) %>%
  summarize(daily_cases = n()) %>%
  filter(age_class %in% c(6,7,8,9,10,11,12)) %>%
  group_by(time) %>%
  summarize(daily_cases = sum(daily_cases))

# plots
plot(young$time, young$daily_cases, xlab="time", ylab="cases", type = "l", ylim = c(0,2500))
points(middle_age$time, middle_age$daily_cases, type="l", col=2)
points(old$time, old$daily_cases, type="l", col=3)

# moving avg
young$avg_cases <- movavg(young$daily_cases, n= 7, type="s")
middle_age$avg_cases <- movavg(middle_age$daily_cases, n= 7, type="s")
old$avg_cases <- movavg(old$daily_cases, n= 7, type="s")

# smooth plots
plot(young$time, young$avg_cases, xlab="time", ylab="cases", type = "l", ylim = c(0,2100))
points(middle_age$time, middle_age$avg_cases, type="l", col=2)
points(old$time, old$avg_cases, type="l", col=3)


#############
### ARIMA ###
#############

data_ts <- ts(data_daily$daily_cases)
plot(data_ts)


# remove weekly seasonality with moving avg
library(TTR)
library(forecast)

data_ts <- SMA(data_ts,n=7)
plot(data_ts,type="l")

#2 ways to remove trend : subtract or differentiate
#1
#model_time_2$fitted.values[7:199] #CAREFUL: last 3 values are NA
detrend1 <- data_ts[7:199] - model_time_2$fitted.values[7:199]
par(mfrow=c(1,2))
plot(detrend1,type="l")

#2
detrend2 <- diff(data_ts[7:199],diff=2)
plot(detrend2,type="l")
par(mfrow=c(1,1))

# now to choose the model look at acf and pacf
acf(detrend2)
pacf(detrend2) #Girls, che minchia è questo?
# seems an AR(2) -> ARIMA(2,1,0)

#but trying to use auto selection
#model <- auto.arima(data_ts,trace=TRUE)
#summary(model)

##### Train/Test split for ARIMA #####
training_set_arima <- data_ts[1:(days-test_days)]
test_set_arima <- data_ts[(days-test_days+1):days]

model <- arima(training_set_arima, order=c(2,1,0))

#predictions
predictions = forecast(model, h=length(test_set_arima), n=30, level = c(80, 95))
plot(predictions)
plot(test_set_arima, type= "l")
points(predictions$mean, type = "l", col = "red")
mse(test_set_arima,predictions$mean) #48615


##### LOGISTIC OR GOMPERTZ ON CUMULATIVE DATA  #####
plot(data_daily$cumulative,type="l")

# fit a logistic function using non linear least squares approximation
logistic_model <- nls(cumulative ~ SSlogis(time, Asym, xmid, scal), data=training_set)
coeff <- coef(logistic_model)
x <- 1:400
plot(x,SSlogis(x,coeff[1],coeff[2],coeff[3]),type="l")
points(data_daily$time, data_daily$cumulative, col=3, pch=1)
points(test_set$time,test_set$cumulative,col=2,pch=20)

#Desumming
desum <- function(model, coeffs, test, training){
  day_test_pred <- model(test$time, coeffs[1], coeffs[2], coeffs[3])
  for (k in length(test$time):2){
    day_test_pred[k] <- day_test_pred[k] - day_test_pred[k-1]
  }
  last_training_day <- max(training$time)
  day_test_pred[1] <- day_test_pred[1] - model(last_training_day,coeff[1],coeff[2],coeff[3])
  
  return(mse(day_test_pred, test$avg_cases))
}

desum(SSlogis, coeff, test_set, training_set) #90k

mse(test_set$cumulative,SSlogis(test_set$time,coeff[1],coeff[2],coeff[3]))
#Here you can see that they seem close but they actually aren't that much.
test_set$cumulative
SSlogis(test_set$time,coeff[1],coeff[2],coeff[3])

# but I want cases to decrease slower
gomp <- function(data,alpha,beta,k){
  return(alpha*exp(-beta*exp(-k*data)))
}
plot(x,gomp(x,300000,188,0.03),type="l")
points(data_daily$time,data_daily$cumulative,col=2,pch=20)

?predict.nls
gomp_model <- nls(cumulative ~ SSgompertz(time,alpha, beta, k), data=training_set)
coeff <- coef(gomp_model)
coeff 
plot(x,SSgompertz(x,coeff[1],coeff[2],coeff[3]),type="l")
points(data_daily$time,data_daily$cumulative,col=2,pch=20)

plot(test_set$time, test_set$cumulative, type= "l")
points(test_set$time,SSgompertz(test_set$time,coeff[1],coeff[2],coeff[3]), type = "l", col = "red")
mse(test_set$cumulative,SSgompertz(test_set$time,coeff[1],coeff[2],coeff[3]))

desum(SSgompertz, coeff, test_set, training_set) #457k
#### GAM ON CUMULATIVE DATA ##### 

#boh boh
model_gam_cumul <- gam(cumulative ~   s(time), method="REML", family = poisson(), data = training_set)
pred <- predict(model_gam_cumul, newdata = test_set, type = "response")
plot(test_set$time, test_set$cumulative, type= "l")
points(test_set$time, pred, type = "l", col = "red")
summary(model_gam)
mse(test_set$cumulative, pred)
