#packages caret and tidyr are already installed

# Attach the dataset to the environment
data(iris)
# Rename the data
iris_dataset<-iris

# Load the Caret package
library(caret)

# Create a partition of 80% training data 20% testing
partition_index <- createDataPartition(iris_dataset$Species, p=0.80, list=FALSE)
# Set 20% of the data for testing
testing <- iris_dataset[-partition_index,]
# Set 80% of data to train the models
training <- iris_dataset[partition_index,]

# Dimensions of the data
dim(training)

# Load ggplot2 library 
library(ggplot2)

# Scatter plot for Petal Length and Width
g <- ggplot(data=training, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Species, shape=Species)) + 
  xlab("Petal Length") + 
  ylab("Petal Width") +
  ggtitle("Petal Length-Width") +
  geom_smooth(method="lm")
print(g)

# Faceting: Producing multiple charts in one plot
facet <- ggplot(data=training, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(aes(shape=Species), size=1.5) +
  geom_smooth(method="lm") +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Faceting") +
  facet_grid(. ~ Species)
print(facet)

# Build and fit model to training set and learn from it
#Decision Tree Model
library(rpart)
model <- rpart(Sepal.Length ~ Sepal.Width, data=training)
print(model)
plot(model)
text(model)
#Random Forest Model
