# predicting two catergory using  catergoriacal predictors  

library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)
head(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
        # adiing new or changing existing colume of a data frame(dataFrame,columnName= funtion(an exiting colume))         
            #here adding a colum 
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  # find row where condition is true(data, logaical based on varibles in data, row evaluate to true are kept)
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

head(dat)

# exploring the data

portionOfFemale <-dat%>% group_by(type) %>% summarise(mean(sex=="Female"))
        # group unique values in to one (variable to group) 
                          # perform operation grouped variables
portionOfFemale 

# predicting female if incalss
predition <- ifelse(dat$type=="inclass","Female","Male")%>% 
  factor(levels = levels(y))
    # use factor to make predition comptibel to confusionMatric funtion 
mean(predition==dat$sex)

#table count each combination of factor levels
table(predition, y)
# same as refernce in confusion matrix 
confusionMatrix(predition,y)
# summarizing the performance of a classification algorithm(factor of predicted class, a facotr of classes to used as true results )


