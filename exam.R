# EXPLORATORY ANALYSIS
setwd("~/Units/StatisticalMethodsforDataScience")
confirmed = read.csv("./Exam/confirmed.csv",header=T)
population = read.csv("./Exam/population.csv",header=T)
names = c("BAJA CALIFORNIA","CHIHUAHUA","COAHUILA","DURANGO","NUEVO LEÓN",
          "SINALOA","SONORA","TAMAULIPAS","ZACATECAS")
data <- confirmed[confirmed$Estado%in%names,-1]
# rename cols
colnames(data) <- c("Region","Sex","Age","Date")

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

######################################
GLM 


