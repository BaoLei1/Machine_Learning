library(tidyverse)
library(HistData)

# regression for continous outcome prediting son's heigh using father heigh 
  # we use lease squares: the best line of fit isthe one that minimized the variance (the sume square of variance) 
galton_heights <- GaltonFamilies %>%
                    # data of parent's height amd childrens height 
   filter(childNum == 1 & gender == "male") %>%
            # the number of  heighse boy in famly =1 means first born 
  select(father, childHeight) %>%
  rename(son = childHeight)

# ml algo that predicts the son's height y, using the father's height x 
library(caret)
y <- galton_heights$son # outcome, son's height 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
head(train_set)

test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg # guessing the avearge height of sons 

mean((avg - test_set$son)^2)
# square loss 

#  use if a pair(X,Y) follow a bivariate normal distribution 
    # the condiotnal expectation is equivalent to teh regression line 
    # f(x)= E(Y|X=x) = beta0+ beta1 *x
fit <- lm(son ~ father, data = train_set)
      # linear regression(outcome~predictor , data ) for esetimating the slope and intercept
fit$coef

# gies us the estimate of the conditonal expectation below 
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
# liner model xonheight = intercept + coefficient * father's heigh 
head(y_hat)

# square loss 
mean((y_hat - test_set$son)^2)
      # prediction - actual height 
# mean suqre error: mesures how close are observed dadta to predited value 
head(test_set)

y_hat <- predict(fit, test_set)
            # retuens a prediction, used insead of a formular for the regression line 
          # gies preditions using (a fitted model, lm or glm or knn etc, data for prdiction )
mean((y_hat - test_set$son)^2)
# mean squre erro 


# read help files
?predict.lm

?predict.glm
# includes a standard error 
?predict





