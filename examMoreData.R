##### Libraries #####

library(dplyr)
library(readr)
library(pracma)
library(rstanarm)
library(loo)
library(Metrics)
library(mgcv)
library(earth)


##### CSV loading #####
data <- read.csv("MexicoCovid19Updated.csv", header = T, sep = ",")
data$Date <- as.Date(data$Date,"%Y-%m-%d")
population = read.csv("./population.csv",header=T)
names = c("BAJA CALIFORNIA","CHIHUAHUA","COAHUILA","DURANGO","NUEVO LEÓN",
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

##### Train/Test split #####
test_days = 5
days <- as.integer(data_daily$time[length(data_daily$time)])
training_set <- data_daily[data_daily$time<=days-test_days,]
test_set <- data_daily[data_daily$time>days-test_days,]


##### Plot and Loss functions #####

# bayesian
loss_stan <- function(model, test){
  pred_model <- posterior_predict(model, newdata = test)
  pred_0025 <- apply(pred_model, 2, function(x) quantile(x, 0.025))
  pred_0975 <- apply(pred_model, 2, function(x) quantile(x, 0.975))
  pred_mean <- apply(pred_model, 2, mean)
  plot(test$time, test$avg_cases, type="l")
  points(test$time, pred_mean, type="l", col="red")
  lines(test$time, pred_0025, lty=2, col="red")
  lines(test$time, pred_0975, lty=2, col="red")
  return(mse(test$avg_cases, pred_mean))
}

# frequentist
loss <- function(model, test){
  pred <- predict(model, newdata = test, type = "response")
  plot(test$time, test$avg_cases, type="l")
  points(test$time, pred, type="l", col="red")
  return(mse(test$avg_cases, pred))
}

#TODO: NEED TO FIGURE THIS OUT
#?loo
#... But let's still use it
#loo_model_time_2 <- loo(model_time_2)
#loo_model_time_2$estimates[3,1] #This yields the LOOIC: the lowest the better

plot(data_daily$time, data_daily$avg_cases, type = "l")
abline(v=c(82, 152), lty=2)

##### Poisson Regression #####

# time
model_time <- stan_glm(avg_cases ~ time, family = poisson,  data=training_set)
loss_stan(model_time, test_set)

# time^2
model_time_2 <- stan_glm( avg_cases ~ time + I(time^2), family = poisson,  data=training_set)
loss_stan(model_time_2, test_set)

# time^3
model_time_3 <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=training_set)
loss_stan(model_time_3, test_set)

# exp(time) !!!!!!
model_time_exp <- glm(avg_cases ~ time + I(time^2) + I(exp(time)), family = poisson,  data=training_set)
loss(model_time_exp, test_set)

# lockdown
model_time_lock <- stan_glm(avg_cases ~ time +  I(time^2) + post_lockdown + lockdown, family = poisson,  data=training_set)
loss_stan(model_time_lock, test_set)

### time to try new stuff

##### GAM ######

#boh boh
model_gam <- gam(avg_cases ~  lag2 + s(time), method="REML", family = poisson(), data = training_set)
pred <- predict(model_gam, newdata = test_set, type = "response")
plot(data_daily$time[180:200], data_daily$avg_cases[180:200], type= "l")
points(test_set$time, pred, type = "l", col = "red")
summary(model_gam)
gam.check(model_gam)
mse(test_set$avg_cases, pred)

##### MARS
mars <- earth(avg_cases ~ time + lag2, data = training_set, nk=5)
pred <- predict(mars, newdata = test_set, type = "response")
plot(data_daily$time[180:200], data_daily$avg_cases[180:200], type= "l")
points(test_set$time, pred, type = "l", col = "red")
mse(test_set$avg_cases, pred)

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

BajaCalifornia <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="BAJA CALIFORNIA")

Chihuahua <- data %>%
  group_by(Region, time) %>%
  summarize(daily_cases = n()) %>%
  filter(Region=="CHIHUAHUA")

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
detrend1 <- data_ts - model_time_2$fitted.values[7:199]
par(mfrow=c(1,2))
plot(detrend1,type="l")

#2
detrend2 <- diff(data_ts[7:199],diff=2)
plot(detrend2,type="l")
par(mfrow=c(1,1))

# now to choose the model look at acf and pacf
acf(detrend2)
pacf(detrend2)
# seems an AR(2) -> ARIMA(2,1,0)

#but trying to use auto selection
#model <- auto.arima(data_ts,trace=TRUE)
#summary(model)

model <- arima(data_ts, order=c(2,1,0))

#predictions
predictions = forecast(model, n=30, level = c(80, 95))
plot(predictions)


##### LOGISTIC OR GOMPERTZ ON CUMULATIVE DATA  #####

data_daily$cumulative = cumsum(data_daily$daily_cases)
plot(data_daily$cumulative,type="l")

# fit a logistic function using non linear least squares approximation
logistic_model <- nls(cumulative ~ SSlogis(time, Asym, xmid, scal), data=data_daily)
coeff <- coef(logistic_model)
x <- 1:400
plot(x,SSlogis(x,coeff[1],coeff[2],coeff[3]),type="l")
points(data_daily$time,data_daily$cumulative,col=2,pch=20)

# but I want cases to decrease slower
gomp <- function(data,alpha,beta,k){
  return(alpha*exp(-beta*exp(-k*data)))
}
plot(x,gomp(x,300000,188,0.03),type="l")
points(data_daily$time,data_daily$cumulative,col=2,pch=20)

gomp_model <- nls(cumulative ~ SSgompertz(time,alpha, beta, k), data=data_daily)
coeff <- coef(gomp_model)
coeff 
plot(x,SSgompertz(x,coeff[1],coeff[2],coeff[3]),type="l")
points(data_daily$time,data_daily$cumulative,col=2,pch=20)
