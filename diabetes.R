library(readr)
diabetes=read.csv("E:/Assignments/ASsignment week 12/Desicion trees/Assignment/Diabetes.csv")
summary(diabetes)
diabetes$Class.variable=as.factor(diabetes$Class.variable)
str(diabetes)
attach(diabetes)
table(diabetes$Class.variable)
str(diabetes)
diabetes$Class.variable <- as.factor(diabetes$Class.variable)
sum(is.na(diabetes$Class.variable))
 

# Shuffle the data
dia_rand <- diabetes[order(runif(1000)), ]
str(dia_rand)

# split the data frames
dia_train <- dia_rand[1:900, ]
dia_test  <- dia_rand[901:1000, ]

# check the proportion of class variable
prop.table(table(dia_rand$Class.variable))
prop.table(table(dia_train$Class.variable))
prop.table(table(dia_test$Class.variable))

# Step 3: Training a model on the data
library(C50)

dia_model <- C5.0(dia_train[, -17], dia_train$Class.variable)

windows()
plot(dia_model) 

# Display detailed information about the tree
summary(dia_model)

# Step 4: Evaluating model performance
# Test data accuracy
test_res <- predict(dia_model, dia_test)
test_acc <- mean(dia_test$Class.variable == test_res)
test_acc

# cross tabulation of predicted versus actual classes

library(gmodels)
CrossTable(dia_test$Class.variable, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(dia_model, dia_train)
train_acc <- mean(dia_train$Class.variable == train_res)
train_acc

table(dia_train$Class.variable, train_res)
