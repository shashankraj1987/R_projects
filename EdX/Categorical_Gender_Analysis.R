# Machine Learning in R (Edx_Harvard)

library(caret)
library(dslabs)
library(dplyr)   ## Needed for %>% 
library(purrr)   ## Needed for map_dbl
data(heights)
heights

y <- heights$sex
x <- heights$height

# This is a Categorical Outcome as Y can be only Right or Wrong. 
# Now, we will take this Dataset and Randomly Make Training and Test Set out of these. 
# We set the Seed to a particular value to Generate the same set of Random Data Everytime. 
# We use createDataPartition to partition the data between Training and Test Sets.
# The test set is only for evaluation.
# Evaluating a Result in the Training Set can lead to overfitting. 

set.seed(2)
test_index <- createDataPartition(y,times = 1,p=0.5,list = FALSE)

train_set <- heights[-test_index,]
test_set <- heights[test_index,]

# We start by Guessing the outcome based on the Sample Data we have, using a Function called sample. 
y_hat <- sample(c("Male","Female"),length(test_index),replace = TRUE)

# Factorizing the Dataset
y_hat <- sample(c("Male","Female"),length(test_index),replace = TRUE) %>% factor(levels = levels(test_set$sex))

# We get the overall accuracy by getting the overall proportion of data that is predicted correctly. 
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarise(mean(height),sd(height))

# Now We are predicting the Heights by checking if the heights are within two standard deviations from the Average Male.
y_hat <- ifelse(x>62,"Male","Female") %>% factor(levels = levels(test_set$sex))
y_hat
mean(y == y_hat)


# We now pick 10 different cutoff values for male heights

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff,function(x){
  y_hat<- ifelse(train_set$height>x,"Male","Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

accuracy
plot(accuracy,type='b')
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Using the Generated Best CutOff to test the accuracy in Test Set.

y_hat <- ifelse(test_set$height >best_cutoff,"Male","Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
