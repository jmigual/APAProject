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
data.a.r = reduceData(data.a)
data.b.r = reduceData(data.b)
allData = rbind(data.a.r, data.b.r)
smallData = allData[sample(nrow(allData),1000),]

# Per comprovar les dades s'usará K-Cross Validation

trc = trainControl(method="cv", number = 10, repeats = 2)

cl = makeCluster(detectCores())
registerDoParallel(cl)

#The final values used for the model were sigma = 0.5 and C = 10. 
svm.model = train(target ~ ., data = smallData, method="svmRadial", maxit = 200, trControl = trc,
                    tuneGrid = expand.grid(C=c(0.1, 1, 10), sigma=c(0.5, 2, 10)))
stopCluster(cl)
svmr.pred <- predict(svm.model)
table(smallData$target, svmr.pred)
checkError(svmr.pred,smallData$target,"SVM small data")

#### amb totes les dades
svm.pred <- predict(svm.model, newdata = allData)
print(table(allData$target,svm.pred))
checkError(svm.pred,allData$target,"SVM all data")

#### amb les dades del B
svm.pred <- predict(svm.model, newdata = data.b.r)
print(table(data.b.r$target,svm.pred))
checkError(svm.pred,data.b.r$target,"SVM")

#### amb les dades del A
svm.pred <- predict(svm.model, newdata = data.a.r)
print(table(data.a.r$target,svm.pred))
checkError(svm.pred,data.a.r$target,"SVM")


####### Linear
# Per comprovar les dades s'usará K-Cross Validation

trc = trainControl(method="cv", number = 10, repeats = 2)

cl = makeCluster(detectCores())
registerDoParallel(cl)


svm.model = train(target ~ ., data = smallData, method="svmLinear", maxit = 200, trControl = trc,
                  tuneGrid = expand.grid(C=c(0.1, 1, 10)))
stopCluster(cl)
svm.pred <- predict(svm.model)
table(smallData$target, svm.pred)
checkError(svmr.pred,smallData$target,"SVM small data")

#### amb totes les dades
svm.pred <- predict(svm.model, newdata = allData)
print(table(allData$target,svm.pred))
checkError(svm.pred,allData$target,"SVM all data")

#### amb les dades del B
svm.pred <- predict(svm.model, newdata = data.b.r)
print(table(data.b.r$target,svm.pred))
checkError(svm.pred,data.b.r$target,"SVM")

#### amb les dades del A
svm.pred <- predict(svm.model, newdata = data.a.r)
print(table(data.a.r$target,svm.pred))
checkError(svm.pred,data.a.r$target,"SVM")
