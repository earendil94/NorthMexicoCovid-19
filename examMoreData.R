#A random 120 year old person appears, gratz mate!
library(dplyr)
library(readr)
library(pracma)
library(rstanarm)

?read_csv
data <- read.csv("MexicoCovid19Updated.csv", header = T, sep = ",")
data$Date <- as.Date(data$Date,"%Y-%m-%d")
max(data$Age)

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

head(data_daily)

no_lock <- rep(0,82)
yes_lock <- seq(1,70)
no_lock_again <- rep(0,48)
lockdown <- c(no_lock, yes_lock, no_lock_again)
data_daily$lockdown <- lockdown

no_lock <- rep(0,152)
post_lock <- seq(1,48)
post_lockdown <- c(no_lock, post_lock)
data_daily$post_lockdown <- post_lockdown

#Daily data show a strong periodicity due to (possibly) sunday testing?
plot(data_daily$time, data_daily$daily_cases, type = "l")

#Moving averages make them smooth
View(data_daily)
?movavg
data_daily$avg_cases <- movavg(data_daily$daily_cases, n= 7, type="s")
plot(data_daily$time, data_daily$avg_cases, type = "l")


#First stan model
data_daily$avg_cases <- as.integer(data_daily$avg_cases)
data_daily$time <- as.integer(data_daily$time)
model1 <- stan_glm( avg_cases ~ time + I(time^2), family = poisson,  data=data_daily)

model1

#Fit
plot(data_daily$time, data_daily$avg_cases, type = "l")
points(data_daily$time, model1$fitted.values, type = "l", col = "red")

#CI
model1$stan_summary
