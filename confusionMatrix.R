library(tidyverse)
library(caret)
library(dslabs)
data(heights)


# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
              # seperate data(a vector of outcoomes, number of partitions to create, percentage of partion.)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]


# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
mean(y_hat==y)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
  #setting levels( levels= setting the  unique values or characters the data has given  )
mean(y_hat==y)


# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
            # group defined variables(sex), where operations are perform by group(data, varibles to group by)
                              # performing groupwise summaries(data, arguments)
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
          # generates a aequence(from, to, increment)
accuracy <- map_dbl(cutoff, function(x){
            #  It takes a vector and a function, calls the function once for each element of the vector
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
            #if(height>cutoff  , true, not true)
    factor(levels = levels(test_set$sex))
      # defining unique levels using factor(data,levels=)
  mean(y_hat == train_set$sex)
  # percentage of y hat predicted true 
})

data.frame(cutoff, accuracy) %>% 
  #putting data into columns (data1,data2,...)
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)


best_cutoff <- cutoff[which.max(accuracy)]
                # index the max accuracy to find the cutoff that gives max accuracy          
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
#
test_set %>% 
  mutate(y_hat = y_hat) %>%
  # adding y_hat col to test_set
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")
            #y is height$heights
confusionMatrix(data = y_hat, reference = test_set$sex)
#Sensitivity: is the proportion of actual positive outcomes correctly identified as such
#specificity:is the proportion of actual negative outcomes that are correctly identified as such.
#Prevalence: percentage of yes occur in our sample (in this case percentafe of femal) 
# https://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/ 

