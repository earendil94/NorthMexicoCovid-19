##### Libraries #####

library(dplyr)
library(readr)
library(pracma)
library(rstanarm)
library(loo)
library(Metrics)


##### CSV loading #####
data <- read.csv("MexicoCovid19Updated.csv", header = T, sep = ",")
data$Date <- as.Date(data$Date,"%Y-%m-%d")

#A random 120 year old person appears, gratz mate!
max(data$Age)

for (i in names){
  data$population[data$Region==i] <- population[population$Estado==i,2]
}

##### Preliminary plotting #####
# hist sex
plot(data$Sex)

#hist age 0-20 20-30 30-40 40-50 50-60 60-70 70-80 80+
data$age_class <- factor(data$Age%/%10)
plot(data$age_class)

#cumulative
data$time <- data$Date-data$Date[1]
plot(data$time,rownames(data),type="l")

# by regions
plot(data$Region)

#Daily data
data_daily <- data %>%
  group_by(time) %>%
  summarize(daily_cases = n())


##### Adding lockdown and post_lockdown columns #####
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

#Daily data show a strong periodicity due to (possibly) sunday testing?
plot(data_daily$time, data_daily$daily_cases, type = "l")

#Moving averages make them smooth
#View(data_daily)
data_daily$avg_cases <- movavg(data_daily$daily_cases, n= 7, type="s")
data_daily$avg_cases <- as.integer(data_daily$avg_cases)
data_daily$time <- as.integer(data_daily$time)
plot(data_daily$time, data_daily$avg_cases, type = "l")

##### Train/Test split #####

test_days = 5
days <- as.integer(data_daily$time[length(data_daily$time)])
training_set <- data_daily[data_daily$time<=days-test_days,]
test_set <- data_daily[data_daily$time>days-test_days,]


##### Poisson regression #####

loss <- function(model, test){
  pred_model <- posterior_predict(model, newdata = test)
  pred_0025 <- apply(pred_model, 2, function(x) quantile(x, 0.025))
  pred_0975 <- apply(pred_model, 2, function(x) quantile(x, 0.975))
  pred_model <- apply(pred_model, 2, mean)
  plot(test$time, test$avg_cases, type="l")
  points(test$time, pred_model, type="l", col="red")
  lines(test$time, pred_0025, lty=2, col="red")
  lines(test$time, pred_0975, lty=2, col="red")
  return(mse(test$avg_cases, pred_model))
}


#TODO: NEED TO FIGURE THIS OUT
#?loo
#... But let's still use it
#loo_model_time_2 <- loo(model_time_2)
#loo_model_time_2$estimates[3,1] #This yields the LOOIC: the lowest the better

#Plot of the fit in red
#plot(data_daily$time, data_daily$avg_cases, type = "l")
#points(data_daily$time, model_time_2$fitted.values, type = "l", col = "red")

#CI
#model_time_2$stan_summary


plot(data_daily$time, data_daily$avg_cases, type = "l")
abline(v=c(82, 152), lty=2)

# time
model_time <- stan_glm(avg_cases ~ time, family = poisson,  data=training_set)
loss(model_time, test_set)

# time^2
model_time_2 <- stan_glm( avg_cases ~ time + I(time^2), family = poisson,  data=training_set)
loss(model_time_2, test_set)
model_time_2$coefficients

# time^3
model_time_3 <- stan_glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=training_set)
loss(model_time_3, test_set)

# exp(time) !!!!!!!!!!!!!!!!!!
# 
?stan_glm
model_time_exp <- stan_glm(avg_cases ~ time + I(time^2) + I(exp(time)), family = poisson,  data=training_set)
mse(test_set$avg_cases, predict.glm(model_time_exp, newdata = test_set, type = "response"))
test_set$avg_cases
plot(training_set$time, training_set$avg_cases, type="l")
points(training_set$time, model_time_exp$fitted.values, type="l", col="red")
points(test_set$time, predict(model_time_exp, newdata = test_set, type="response"), type="l", col="red")
loss(model_time_exp, test_set)

# lockdown
model_time_lock <- stan_glm(avg_cases ~ time +  I(time^2) + post_lockdown + lockdown, family = poisson,  data=training_set)
loss(model_time_lock, test_set)

### time to try new stuff