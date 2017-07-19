#load caret package
#library(caret)

#load iris data
data(iris)

#rename the data
iris_dataset <- iris

#split iris data by 80% to train model and 20% to validate
trainIndex <- createDataPartition(iris_dataset$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1);
#set train var = 80% of data
train <- iris_dataset[trainIndex,]
#set validation var = 20% of data
validation <- iris_dataset[-trainIndex,]

#get a summary of mean, median, min, max, etc. values
summary(train)

#store input columns into x
x <- train[,1:4]
#store output columns into y
y <- train[,5]

# #plot all 4 column' data of Train dataset as Pairs Scatterplot Matrix
# pairs(train[,1:4], main="Iris Data", pch=21, bg=c("red", "green", "blue")[unclass(train$Species)])