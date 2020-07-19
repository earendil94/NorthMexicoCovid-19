library(dplyr)

# EXPLORATORY ANALYSIS
setwd("~/Units/StatisticalMethodsforDataScience")
confirmed = read.csv("./confirmed.csv",header=T)
population = read.csv("./population.csv",header=T)
names = c("BAJA CALIFORNIA","CHIHUAHUA","COAHUILA","DURANGO","NUEVO LEÓN",
          "SINALOA","SONORA","TAMAULIPAS","ZACATECAS")
data <- confirmed[confirmed$Estado%in%names,-1]
# rename cols
colnames(data) <- c("Region","Sex","Age","Date")
View(data)
data$Date <- as.Date(data$Date,"%d/%m/%Y") 
# oder by date
data <- data[order(as.Date(data$Date, format="%d/%m/%Y")),]
rownames(data)=1:dim(data)[1]
levels(data$Region) <- droplevels(data$Region)
levels(data$Region)

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

################################################

######      Daily cases kaggle dataset  ########

################################################

View(data)
glm(data=data)

data_cum <- data %>%
  group_by(time) %>%
  summarize(daily_cases = n())

plot(data_cum$time, data_cum$daily_cases)


#####################################

#########  Mexico gov data ##########

#####################################

#Load data
updated_data <- read.csv('200718COVID19MEXICO.csv', header=T)

#Select regions in the north
data_norte <- updated_data %>%
  filter(ENTIDAD_RES  %in% c(2,5,8,10,19,25,26,28,32))

#Select useful covariates
data_light <- data_norte %>%
  select(FECHA_INGRESO, ENTIDAD_RES, SEXO, EDAD)

#Fix names
colnames(data_light) <- c("Date", "Region", "Sex", "Age")
data_light$Region <- factor(x = data_light$Region, labels=c("BAJA CALIFORNIA","CHIHUAHUA","COAHUILA","DURANGO","NUEVO LEON",
                                       "SINALOA","SONORA","TAMAULIPAS","ZACATECAS"))
data_light$Sex <- factor(x = data_light$Sex, labels=c("FEMININO", "MASCULINO"))

# order by date
data_light$Date <- as.Date(data_light$Date,"%Y-%m-%d")
data_light <- data_light[order(as.Date(data_light$Date)),]
rownames(data_light)=1:dim(data_light)[1]



#This is going to update the data that we've considered originally
data <- data_light
write.csv(data, file = "MexicoCovid19Updated.csv", sep = ",", row.names = FALSE)


#A random 120 year old person appears, gratz mate!
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

#Daily data show a strong periodicity due to (possibly) sunday testing?
plot(data_daily$time, data_daily$daily_cases, type = "l")

#Moving averages make them smooth
data_daily$avg_cases <- movavg(data_daily$daily_cases, n= 7, type="s")
plot(data_daily$time, data_daily$avg_cases, type = "l")


