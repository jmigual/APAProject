rm(list = ls())
library(MASS)
library(nnet)
library(caret)
library(doParallel)

source("./R/readAllData.R")

checkError = function(predicted, real, type) {
  tab = table(predicted, real)
  print(paste("Error", type))
  print(tab)
  
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

#######################
# INICI DE L'EXECUCIO #
#######################

# Llegir les dades de training
data.train = readAllData("a", single = FALSE)

# Llegir les dades de testing
data.valid = readAllData("b", single = FALSE)

model.nnet = nnet(target ~ ., data = data.train, size = 10, decay = 0.1, maxit = 200, MaxNWts = 100000)

train.pred = predict(model.nnet, type = "class")
checkError(train.pred, data.train$target, "train")

valid.pred = predict(model.nnet, newdata = data.valid, type = "class")
checkError(valid.pred, data.valid$target, "validation")


trc <- trainControl (method="cv", number=10, repeats=10)

cl = makeCluster(detectCores())
registerDoParallel(cl)

# Best option is size = 5, decay = 0.1
model.train = train (target ~., data = data.train, method='nnet', maxit = 200, trControl=trc, MaxNWts = 10000)

train.car.pred = predict(model.train)
checkError(train.car.pred, data.train$target, "train 2")

stopCluster(cl)
