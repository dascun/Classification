#############################
### Set working directory ###
#############################

getwd() # "C:/Users/Win10/Documents"
setwd("C:/Users/Win10/Desktop/Classification")

#######################
### Import packages ###
#######################

load.libraries <- c('xlsx', 'rpart', 'rpart.plot', 'randomForest', 'e1071', 'class', 'xgboost')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
# incase of any issue with rJava please ref. to https://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/

########################
### Read source file ###
########################

file <- read.xlsx("Immunotherapy.xlsx", 1, header = TRUE)

# Exploratory Analysis - Not required here

# dim(file)
# str(file)
# colSums(sapply(file, is.na))
# https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
# table(file$sex, file$Result_of_Treatment)

# Split file data into train and test dataset

set.seed(1)
n <- nrow(file)
smp_size <- floor(0.9 * n)
train_ind <- sample(seq_len(n), size = smp_size)
train <- file[train_ind, ]
test <- file[-train_ind, ]

####################################################
### Logistic Regression Classification Algorithm ###
####################################################

# Build model

model <- glm(Result_of_Treatment ~ ., train, family = "binomial")

# Predict outcome of test dataset

pred.lr <- predict(model, test, type = "response")

# Building the confusion Matrix

conf.lr <- table(test$Result_of_Treatment, pred.lr)

# Accuracy of prediction

accuracy.lr <- sum(diag(conf.lr))/sum(conf.lr)
print(accuracy.lr)

##############################################
### Decision Tree Classification Algorithm ###
##############################################

# Build model

tree <- rpart(Result_of_Treatment ~ ., train, method = "class")
# rpart.plot(tree)

# Predict outcome of test dataset

pred <- predict(tree, test, type = "class")

# Building the confusion Matrix

conf <- table(test$Result_of_Treatment, pred)

# Accuracy of prediction

accuracy <- sum(diag(conf))/sum(conf)
print(accuracy)

# Cross validation and Pruning - Not required here
# printcp(tree)
# plotcp(tree)
# ptree<- prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
# rpart.plot(ptree)
# ppred <- predict(ptree, test, type = "class")
# pconf <- table(test$Result_of_Treatment, ppred)
# paccuracy <- sum(diag(pconf))/sum(pconf)
# print(paccuracy)

##############################################
### Random Forest Classification Algorithm ###
##############################################

# Build model

class(train$Result_of_Treatment) # numeric
train$Result_of_Treatment <- factor(train$Result_of_Treatment)
tree.rf <- randomForest(Result_of_Treatment ~ ., train)

# Predict outcome of test dataset

pred.rf <- predict(tree.rf, test)

# Building the confusion Matrix

conf.rf <- table(test$Result_of_Treatment, pred.rf)

# Accuracy of prediction

accuracy.rf <- sum(diag(conf.rf))/sum(conf.rf)
print(accuracy.rf)

############################################
### Naive Bayes Classification Algorithm ###
############################################

# Build model

model.nb <- naiveBayes(Result_of_Treatment ~ ., train)

# Predict outcome of test dataset

pred.nb <- predict(model.nb, test)

# Building the confusion Matrix

conf.nb <- table(test$Result_of_Treatment, pred.nb)

# Accuracy of prediction

accuracy.nb <- sum(diag(conf.nb))/sum(conf.nb)
print(accuracy.nb)

####################################
### KNN Classification Algorithm ###
####################################

# Build model and predict outcome of test dataset

train_lbl <- train$Result_of_Treatment
pred.knn <- knn(train, test, train_lbl, k=10)

# Building the confusion Matrix

conf.knn <- table(test$Result_of_Treatment, pred.knn)

# Accuracy of prediction

accuracy.knn <- sum(diag(conf.knn))/sum(conf.knn)
print(accuracy.knn)

########################################
### XGBOOST Classification Algorithm ###
########################################

# Build model
train_l <- as.numeric(levels(train_lbl))[train_lbl]
xgb <- xgboost(data.matrix(train[,-8]), train_l, nround=10, objective = "binary:logistic")

# Predict outcome of test dataset

pred.xgb <- predict(xgb, data.matrix(test[,-8]))

# Building the confusion Matrix

conf.xgb <- table(test$Result_of_Treatment, pred.xgb)

# Accuracy of prediction

accuracy.xgb <- sum(diag(conf.xgb))/sum(conf.xgb)
print(accuracy.xgb) # powerful but not efficient for small data, can be improved by cv