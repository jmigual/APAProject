rm(list = ls())
library(MASS)
library(kernlab)
library(caret)
library(doParallel)

source("./R/readAllData.R")
source("./R/reduceData.R")

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
data.a = readAllData("a", single = FALSE)
data.b = readAllData("b", single = FALSE)
allData = reduceData(rbind(data.a, data.b))
smallData = allData[sample(nrow(allData),3000),]

# Per comprovar les dades s'usará K-Cross Validation

trc = trainControl(method="cv", number = 10, repeats = 2)

cl = makeCluster(detectCores())
registerDoParallel(cl)

#The final values used for the model were sigma = 0.5 and C = 10. 
train.model = train(target ~ ., data = smallData, method="svmRadial", maxit = 200, trControl = trc,
                    tuneGrid = expand.grid(C=c(0.1, 1, 10), sigma=c(0.5, 2, 10)))
stopCluster(cl)
svmr.pred <- predict(train.model)
table(smallData$target, svmr.pred)
checkError(svmr.pred,smallData$target,"SVM")

#svm.model <- svm(target ~ .,data = smallData, type="C-classification", kernel="radial",cost = 10, gamma = 0.5)


train.pred = predict(model.nnet, type = "class")
checkError(train.pred, allData$target, "train")

