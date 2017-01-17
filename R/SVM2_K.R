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
svm.model = train(target ~ ., data = smallData, method="svmRadial", maxit = 200, trControl = trc,
                    tuneGrid = expand.grid(C=c(0.1, 1, 10), sigma=c(0.5, 2, 10)))
stopCluster(cl)
svmr.pred <- predict(svm.model)
table(smallData$target, svmr.pred)
checkError(svmr.pred,smallData$target,"SVM small data")

#### amb totes les dades
svm.pred <- predict(svm.model, newdata = allData)
print(table(data.b$target,svm.pred))
checkError(svm.predb,allData$target,"SVM all data")

#### amb les dades del B
svm.predb <- predict(svm.model, newdata = data.b)
print(table(data.b$target,svmp.predb))
checkError(svm.predb,data.b$target,"SVM")

#### amb les dades del A
svp.preda <- predict(svm.model, newdata = data.a)
print(table(data.b$target,svmp.predb))
checkError(svmp.preda,data.b$target,"SVM")


####### Linear
# Per comprovar les dades s'usará K-Cross Validation

trc = trainControl(method="cv", number = 10, repeats = 2)

cl = makeCluster(detectCores())
registerDoParallel(cl)


svm.model = train(target ~ ., data = smallData, method="svmLinear", maxit = 200, trControl = trc,
                  tuneGrid = expand.grid(C=c(0.1, 1, 10)))
stopCluster(cl)
svmr.pred <- predict(svm.model)
table(smallData$target, svmr.pred)
checkError(svmr.pred,smallData$target,"SVM small data")

#### amb totes les dades
svm.pred <- predict(svm.model, newdata = allData)
print(table(allData$target,svm.pred))
checkError(svm.pred,allData$target,"SVM all data")

#### amb les dades del B
svm.predb <- predict(svm.model, newdata = data.b)
print(table(data.b$target,svmp.predb))
checkError(svm.predb,data.b$target,"SVM")

#### amb les dades del A
svp.preda <- predict(svm.model, newdata = data.a)
print(table(data.b$target,svmp.predb))
checkError(svmp.preda,data.b$target,"SVM")




