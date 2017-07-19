#import dataset
library(readr)
FlightDelays <- read_csv("~/R/FlightDelays.csv")

library(ggplot2)
#Set g to plot flight status
gg <- ggplot(FlightDelays, aes(FlightStatus))
#Plot barchart
gg + geom_bar()

#Create dummy values for 4 categorical values
f.dummy <- model.matrix(~CARRIER+DEST+ORIGIN, data=FlightDelays)
#Remove the first column since 'Intercept' is unnecessary
f.dummy <- f.dummy[,-1]
#View top 5 rows
head(f.dummy, 5)

#Add Dist, FL_NUM, Weather and FlightStatus from Sub to combined dummy matrix
s <- FlightDelays[,c(5,7,9,13)]
#Combine into one matrix
f_dummy <- cbind(f.dummy,s)
head(f_dummy,5)

library(caret)
# 60% of the sample size
sample_size <- floor(0.60 * nrow(f_dummy))

## set the seed to make your partition reproductible
set.seed(2)
# Create a partition of 60% training data 40% testing
part_index <- sample(seq_len(nrow(f_dummy)), size = sample_size)
# Set 60% of data to train the models
train <- f_dummy[part_index,]
# Set 40% of the data for testing
test <- f_dummy[-part_index,]

###### TRAINING #######

#Feed training data (60%) into logistic regression model
mod <- glm(formula = as.factor(FlightStatus) ~ ., family = binomial, data = train)
summary(mod)
#Find each training entry's prediction probability
pred <- predict(mod, type='response')

#confusion matrix with TP, FP, TN, FN
c <- table(train$FlightStatus, pred>0.5)
c

#Precision = TP/(TP+FP)
prec <- c[1,1]/(c[1,1] + c[1,2])
sprintf("Precision = %s", prec)

#Recall = TP/(TP + FN)
rec <- c[1,1]/(c[1,1] + c[2,1])
sprintf("Recall = %s", rec)

#Calculate and plot ROC curve
library(gplots)
library(ROCR)
library(caTools)
library(ModelMetrics)
#Classified prediction of on time or delayed
ROCRprediction <- prediction(pred, train$FlightStatus)
ROCRperformance <- performance(ROCRprediction, 'tpr', 'fpr')
plot(ROCRperformance)
Auc <- auc(train$FlightStatus, pred)
sprintf("AUC = %s", Auc)

##### TESTING ######
#Feed training data (60%) into logistic regression model
mod <- glm(formula = as.factor(FlightStatus) ~ ., family = binomial, data = test)
summary(mod)
#Find each training entry's prediction probability
pred <- predict(mod, type='response')

#confusion matrix with TP, FP, TN, FN
c <- table(test$FlightStatus, pred>0.5)

#Precision = TP/(TP+FP)
prec <- c[1,1]/(c[1,1] + c[1,2])
sprintf("Precision = %s", prec)

#Recall = TP/(TP + FN)
rec <- c[1,1]/(c[1,1] + c[2,1])
sprintf("Recall = %s", rec)

#Calculate and plot ROC curve
library(gplots)
library(ROCR)
#Classified prediction of on time or delayed
ROCRprediction <- prediction(pred, test$FlightStatus)
ROCRperformance <- performance(ROCRprediction, 'tpr', 'fpr')
plot(ROCRperformance)
Auc <- auc(test$FlightStatus, pred)
sprintf("AUC = %s", Auc)

#Based on AUC values, looks like the first Model (from LogisticRegression.R) performs better
anova(model, mod, test="Chisq")

