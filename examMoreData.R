library(dplyr)
library(pracma)
library(rstanarm)

setwd("/Users/solonicolas/DSSC/NorthMexicoCovid-19")
data <- read.csv("MexicoCovid19Updated.csv", header = T, sep = ",")
population = read.csv("./population.csv",header=T)

names = c("BAJA CALIFORNIA","CHIHUAHUA","COAHUILA","DURANGO","NUEVO LEON","SINALOA","SONORA","TAMAULIPAS","ZACATECAS")
data$Date <- as.Date(data$Date,"%Y-%m-%d")

#A random 120 year old person appears, gratz mate!
max(data$Age)

first_day_data <- min(data$Date)
lockdown_start <- as.Date("2020-03-23")
lockdown_end <- as.Date("2020-06-01")
last_day_data <- max(data$Date)

for (i in names){
  data$population[data$Region==i] <- population[population$Estado==i,2]
}

dim(data)
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

#Adding lockdown and post_lockdown columns
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
View(data_daily)
data_daily$avg_cases <- movavg(data_daily$daily_cases, n= 7, type="s")
plot(data_daily$time, data_daily$avg_cases, type = "l")

################################################################################
################### MODELS #####################################################
################################################################################

data_daily$avg_cases <- as.integer(data_daily$avg_cases)
data_daily$time <- as.integer(data_daily$time)

#First stan model

model1 <- stan_glm( avg_cases ~ time + I(time^2), family = poisson,  data=data_daily)

model1

#Fit
plot(data_daily$time, data_daily$avg_cases, type = "l")
points(data_daily$time, model1$fitted.values, type = "l", col = "red")

#CI
model1$stan_summary

# Not stan

plot(data_daily$time, data_daily$avg_cases, type = "l")
abline(v=c(82, 152), lty=2)

# time
model2 <- glm(avg_cases ~ time, family = poisson,  data=data_daily)
model2
points(data_daily$time, model2$fitted.values, type = "l", col = 2)

# time^2
model3 <- glm(avg_cases ~ time + I(time^2), family = poisson,  data=data_daily)
model3
points(data_daily$time, model3$fitted.values, type = "l", col = 3)

# time^3
model4 <- glm(avg_cases ~ time + I(time^2) + I(time^3), family = poisson,  data=data_daily)
model4
points(data_daily$time, model4$fitted.values, type = "l", col = 4)

# exp(time) !!!!!!!!!!!!!!!!!!
model5 <- glm(avg_cases ~ time + I(time^2) + I(exp(time)), family = poisson,  data=data_daily)
model5
points(data_daily$time, model5$fitted.values, type = "l", col = 5)

# lockdown
model6 <- glm(avg_cases ~ time +  I(time^2) + post_lockdown + lockdown, family = poisson,  data=data_daily)
model6
points(data_daily$time, model6$fitted.values, type = "l", col = 6)

################################################################################
################### TEST-TRAIN SPLIT ###########################################
################################################################################

# train-test split 
test_days = 20
days <- as.integer(data$time[length(data$time)])
training_set <- data[data$time<days-test_days,]
test_set <- data[data$time>=days-test_days,]

data_daily_train <- training_set %>%
  group_by(time) %>%
  summarize(daily_cases = n())

data_daily_test <- test_set %>%
  group_by(time) %>%
  summarize(daily_cases = n())
