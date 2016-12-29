rm(list = ls())
library(MASS)
library(nnet)
library(caret)
library(doParallel)

source("./R/readAllData.R")

checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels=0:1), real)
  print(paste("Error", type))
  print(tab)
  
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

#######################
# INICI DE L'EXECUCIO #
#######################

# Llegir les dades de training
data.train = readAllData("a", single = TRUE)

# Llegir les dades de testing
data.valid = readAllData("b", single = TRUE)

model.nnet = nnet(target ~ ., data = data.train, size = 15, decay = 0.01, maxit = 200, MaxNWts = 10000)

train.pred = predict(model.nnet, type = "class")
checkError(train.pred, data.train$target, "train")

valid.pred = predict(model.nnet, newdata = data.valid, type = "class")
checkError(valid.pred, data.valid$target, "validation")


trc <- trainControl (method="repeatedcv", number=10, repeats=10)

cl = makeCluster(detectCores())
registerDoParallel(cl)
model.train = train (target ~., data = data.train, method='nnet', maxit = 200, trControl=trc, MaxNWts = 10000)
stopCluster(cl)
