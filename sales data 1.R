library(readr)
sales=read.csv("E:/Assignments/ASsignment week 12/Desicion trees/Assignment/Company_Data.csv")
summary(sales)
sales$Urban=as.factor(sales$Urban)
sales$US=as.factor(sales$US)
sales$ShelveLoc=as.factor(sales$ShelveLoc)
library(caTools)
set.seed(0)
attach(sales)
split <- sample.split(sales$Sales, SplitRatio = 0.8)
sales_train <- subset(sales, split == TRUE)
sales_test <- subset(sales, split == FALSE)

library(rpart.plot)
library(rpart)
model <- rpart(sales_train$Sales ~ ., data = sales_train,
               control = rpart.control(cp = 0, maxdepth = 3))
rpart.plot(model, box.palette = "auto", digits = -3)
# Measure the RMSE on Test data
test_pred <- predict(model, newdata = sales_test, type = "vector")

# RMSE
rmse1 <- sqrt(mean(sales_test$Sales - test_pred)^2)
rmse1

train_pred <- predict(model, newdata = sales_train, type = "vector")

# RMSE
rmse_train <- sqrt(mean(sales_train$Sales - train_pred)^2)
rmse_train

# Prune the Decision Tree

# Grow the full tree
fullmodel <- rpart(sales_train$Sales ~ ., data = sales_train,
                   control = rpart.control(cp = 0))

rpart.plot(fullmodel, box.palette = "auto", digits = -3)
# Examine the complexity plot
# Tunning parameter check the value of cp which is giving us minimum cross validation error (xerror)
printcp(fullmodel)   
plotcp(model)

mincp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]

# Prune the model based on the optimal cp value
model_pruned_1 <- prune(fullmodel, cp = mincp)
rpart.plot(model_pruned_1, box.palette = "auto", digits = -3)

model_pruned_2 <- prune(fullmodel, cp = 0.02)
rpart.plot(model_pruned_2, box.palette = "auto", digits = -3)
# Measure the RMSE using Full tree
test_pred_fultree <- predict(fullmodel, newdata = sales_test, type = "vector")
# RMSE
rmse_f <- sqrt(mean(sales_test$Sales - test_pred_fultree)^2)
rmse_f
# Measure the RMSE using Prune tree - model1
test_pred_prune1 <- predict(model_pruned_1, newdata = sales_test, type = "vector")
# RMSE
rmse_prune1 <- sqrt(mean(sales_test$Sales - test_pred_prune1)^2)
rmse_prune1




# Prediction for trained data result
train_pred_fultree <- predict(fullmodel, sales_train, type = 'vector')

# RMSE on Train Data
train_rmse_fultree <- sqrt(mean(sales_train$Sales - train_pred_fultree)^2)
train_rmse_fultree


# Prediction for trained data result
train_pred_prune1 <- predict(model_pruned_1, sales_train, type = 'vector')

# RMSE on Train Data
train_rmse_fultree2 <- sqrt(mean(sales_train$Sales - train_pred_prune1)^2)
train_rmse_fultree2

# Prediction for trained data result
train_pred_prune2 <- predict(model_pruned_2, sales_train, type = 'vector')

# RMSE on Train Data
train_rmse_fultree2 <- sqrt(mean(sales_train$Sales - train_pred_prune2)^2)
train_rmse_fultree2


