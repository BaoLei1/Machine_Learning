library(caret)
library(tidyverse)
library(dslabs)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
              # give true indices,pural for index, 
        # removing setosa  
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
              #sepersting data 
# line of code
test <- iris[test_index,]
train <- iris[-test_index,]

head(train)

#*
# find the best preditor amoumt many predictor to predict two cat. outcomes


foo <- function(feature){
        # defind new funciton(name){expression last expression is returned}
  rangedValues <- seq(min(feature) ,max(feature) ,by=0.1)
                   #making a list of values according to data 
  sapply(rangedValues,function(rangedValues){
   # apply a function over a list or vector(a vector,a function to be applie on each element of a vector)
     y_hat <- ifelse(feature>rangedValues,'virginica','versicolor')
              # is the feature is greater than cuttoff, verginica, not versicolor 
     mean(y_hat==train$Species)
      # return poportion of correctly predicted 
     })
}
predictions <- apply(train[,-5],2,foo)
              #applying a function to margins of an array or matrix(
                    # matrix, 
                              #1 indicates rows 2 indicates colume 1&2 indicates rows and columes,
                                  # the function to be applyed )
head(predictions)
#  returning a portion for each cutoff over all specified colum data, so outputs has leght(rangedValues) in each each colum eg.($Sepal.Length)   
sapply(predictions,max)	
# looking for a max on  preditons 


# using samrt cutoff value from training data to calvulate overall accuracy on test set
head(train)
predictions <- foo(train[,3])
                # indexing traning data on 3rd colume
rangedValues <- seq(min(train[,3]) ,max(train[,3]),by=0.1)
                # generating a seq of vacalue
cutoffs <-rangedValues[which(predictions==max(predictions))]
            # lookiing for the best predition on test set

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


#explortory data analysis 
plot(iris,pch=21,bg=iris$Species)
#Petal.Length and Petal.Width in combination could potentially be more information than either feature alone

predictions <- foo(train[,3] )
# indexing traning data on 3rd colume
rangedValues <- seq(min(train[,3] ) ,max(train[,3] ),by=0.1)
# generating a seq of vacalue
cutoffs <-rangedValues[which(predictions==max(predictions))]
# lookiing for the best predition on test set

predictions <- foo(train[,4] )
# indexing traning data on 3rd colume
rangedValues <- seq(min(train[,4] ) ,max(train[,4] ),by=0.1)
# generating a seq of vacalue
cutoffss <-rangedValues[which(predictions==max(predictions))]
# lookiing for the best predition on test set
cutoffss


y_hat <- ifelse(test[,3]>cutoffs[1]|test[,4]>cutoffss[1],'virginica','versicolor')

mean(y_hat==test$Species)


# up to here
# solution on hw 

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)



