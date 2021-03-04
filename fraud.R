library(readr)
fraud=read.csv("E:/Assignments/ASsignment week 12/Desicion trees/Assignment/Fraud_check.csv")
summary(fraud)
fraud$Undergrad=as.factor(fraud$Undergrad)
fraud$Urban=as.factor(fraud$Urban)
str(fraud)

fraud$output <- ifelse(fraud$Taxable.Income <= 30000,"Risky","Good")
View(fraud)
str(fraud)
fraud$output <- as.factor(fraud$output)
library(C50)
mydata_risky <- fraud[fraud$output == "Risky","output"]
mydata_risky
mydata_good <- fraud[fraud$output == "Good","output"]
mydata_good
library(caTools)

set.seed(0)
split <- sample.split(fraud$output, SplitRatio = 0.8)
fraud_train <- subset(fraud, split == TRUE)
fraud_test <- subset(fraud, split == FALSE)
library(rpart)
# check the proportion of class variable
prop.table(table(fraud$Urban))
prop.table(table(fraud_train$Urban))
prop.table(table(fraud_test$Urban))
prop.table(table(fraud_test$output))
library(C50)

fraud_model <- C5.0(fraud_train[, -17], fraud_train$output)

windows()
plot(fraud_model) 
summary(fraud_model)


test_res <- predict(fraud_model, fraud_test)
test_acc <- mean(fraud_test$output == test_res)
test_acc
library(gmodels)
CrossTable(fraud_test$output, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(fraud_model, fraud_train)
train_acc <- mean(fraud_train$output == train_res)
train_acc

table(fraud_train$output, train_res)

