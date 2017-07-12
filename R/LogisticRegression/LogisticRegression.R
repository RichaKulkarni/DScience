#import dataset
library(readr)
FlightDelays <- read_csv("~/R/FlightDelays.csv")

library(ggplot2)
#Set g to plot flight status
g <- ggplot(FlightDelays, aes(FlightStatus))
#Plot barchart
g + geom_bar()

#Convert column type from Int to Char to make categorical for model.matrix
FlightDelays$DAY_WEEK <- as.character(FlightDelays$DAY_WEEK)
#Create dummy values for 4 categorical values
flights.dummy <- model.matrix(~CARRIER+DEST+ORIGIN+DAY_WEEK, data=FlightDelays)
#Remove the first column since 'Intercept' is unnecessary
flights.dummy <- flights.dummy[,-1]
#View top 5 rows
head(flights.dummy, 5)

#Two ways to cut DEP_TIME into categorical buckets by the hour (hourly military time)
FlightDelays$bucketDEP_TIME <- cut(FlightDelays$DEP_TIME, breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400), labels=c("HourBlockDeptTimeHourBlock0","HourBlockDeptTimeHourBlock1","HourBlockDeptTimeHourBlock2","HourBlockDeptTimeHourBlock3","HourBlockDeptTimeHourBlock4","HourBlockDeptTimeHourBlock5","HourBlockDeptTimeHourBlock6","HourBlockDeptTimeHourBlock7","HourBlockDeptTimeHourBlock8","HourBlockDeptTimeHourBlock9","HourBlockDeptTimeHourBlock10","HourBlockDeptTimeHourBlock11","HourBlockDeptTimeHourBlock12","HourBlockDeptTimeHourBlock13","HourBlockDeptTimeHourBlock14","HourBlockDeptTimeHourBlock15","HourBlockDeptTimeHourBlock16","HourBlockDeptTimeHourBlock17","HourBlockDeptTimeHourBlock18","HourBlockDeptTimeHourBlock19","HourBlockDeptTimeHourBlock20","HourBlockDeptTimeHourBlock21","HourBlockDeptTimeHourBlock22","HourBlockDeptTimeHourBlock23"))
#FlightDelays$testingCut <- cut(FlightDelays$DEP_TIME, seq(0,2400,by=100))

#Convert categorical buckets into dummy variables
flights.dummy2 <- model.matrix(~bucketDEP_TIME, data=FlightDelays)
#Remove the first column since 'Intercept' is unnecessary
flights.dummy2 <- flights.dummy2[,-1]

#Combine the two dummy matrices into one
flights_dummy <- cbind(flights.dummy, flights.dummy2)
#Add Weather and FlightStatus from Sub to combined dummy matrix
sub <- FlightDelays[,c(9,13)]
flights_dummy <- cbind(flights_dummy,sub)
head(flights_dummy,5)

library(caret)
# 60% of the sample size
smp_size <- floor(0.60 * nrow(flights_dummy))

## set the seed to make your partition reproductible
set.seed(1)
# Create a partition of 60% training data 40% testing
partition_index <- sample(seq_len(nrow(flights_dummy)), size = smp_size)
# Set 60% of data to train the models
training <- flights_dummy[partition_index,]
# Set 40% of the data for testing
testing <- flights_dummy[-partition_index,]

###### TRAINING #######

#Feed training data (60%) into logistic regression model
model <- glm(formula = as.factor(FlightStatus) ~ ., family = binomial, data = training)
summary(model)
#Find each training entry's prediction probability
predict <- predict(model, type='response')

#confusion matrix with TP, FP, TN, FN
table(training$FlightStatus, predict>0.5)

#Calculate and plot ROC curve
library(gplots)
library(ROCR)
#Classified prediction of on time or delayed
ROCRpred <- prediction(predict, training$FlightStatus)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf)

##### TESTING ######
#Feed training data (60%) into logistic regression model
model <- glm(formula = as.factor(FlightStatus) ~ ., family = binomial, data = testing)
summary(model)
#Find each training entry's prediction probability
predict <- predict(model, type='response')

#confusion matrix with TP, FP, TN, FN
table(testing$FlightStatus, predict>0.5)

#Calculate and plot ROC curve
library(gplots)
library(ROCR)
#Classified prediction of on time or delayed
ROCRpred <- prediction(predict, testing$FlightStatus)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf)
