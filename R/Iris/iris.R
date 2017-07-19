#load caret package
library(caret)

#load iris data
data(iris)

#Ex - simple linear regression with lm
#plot simple graph of petal length vs. petal width and differentiate three species by color
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red", "green", "blue")[unclass(iris$Species)], main="Iris Data", xlab="Petal Length", ylab="Petal Width")
#use linear model to model petal width depending on petal length
lm(Petal.Width ~ Petal.Length, data=iris)$coefficients
#plot lm line to to graph
abline(lm(Petal.Width ~ Petal.Length, data=iris)$coefficients, col="black")
#get summary of data
summary(lm(Petal.Width ~ Petal.Length, data=iris))

#Ex - linear regression on Sepal Width and Length; lm does not fit well on total data
#plot simple graph of sepal width vs. sepal length and differentiate three species by color
plot(iris$Sepal.Width, iris$Sepal.Length, pch=21, bg=c("red", "green", "blue") [unclass(iris$Species)], main="Iris Data", xlab="Sepal Width", ylab="Sepal Length")
#plot lm line (where model sepal length depending on sepal width) to to graph
abline(lm(Sepal.Length ~ Sepal.Width, data=iris)$coefficients, col="black")
#three separate linear regressions
abline(lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="setosa"),])$coefficients, col="red")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="versicolor"),])$coefficients, col="green")
abline(lm(Sepal.Length ~ Sepal.Width, data=iris[which(iris$Species=="virginica"),])$coefficients, col="blue")




