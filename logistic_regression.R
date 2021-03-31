# logistic regression is an extension of linear regression that estimate of conditonal probability is between 0 and 1
  # we no longer use the least squares
  # we suse maximum likelihood estimate 
library(dslabs)
library(caret)
library(tidyverse)

heights %>% 
  mutate(x = round(height)) %>%
            #rounding height to digit = 0 
  group_by(x) %>%
    # group by height 
  filter(n() >= 10) %>%
    # filtering   height has rows grater than 10
  summarize(prop = mean(sex == "Female")) %>% 
                  # retruning the overgae female hieght in each group 
  ggplot(aes(x, prop)) +
            # plotting the x= height , y is the average female per grouop 
  geom_point() + 
  
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
  # drwing a line          # x when y =0,            # slop is how much y moves if x moves by a number 
                          # lm.fit  linear models
range(p_hat)

head(train_set)
# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
              # making a new variable call y representing female as 1 male as 0
  glm(y ~ height, data=., family = "binomial")
      # outcome~height ,   family = binomial, gaussian, Gama etc
                            # either think of it as sucess or faliure
  # glm 
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
                                                        # conditional probability 
              # predicting from the results of various differnt model
                        #
tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female"))   
 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
                                  # from min height ro max height increment by 1 
              
  mutate(p_hat = plogis(glm.fit$coef[1] + glm.fit$coef[2]*x))
                  # logistic density distribution function (y intercept + slop * roeunded height)
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
