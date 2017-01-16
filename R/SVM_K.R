# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
library(doParallel)

source("./R/readAllData.R")

checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels(real)), real)
  print(paste("Error", type))
  print(tab)
  
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

#######################
# INICI DE L'EXECUCIO #
#######################

# Llegir les dades de training
dataA = readAllData("a", single = FALSE)

# Llegir les dades de testing
dataB = readAllData("b", single = FALSE)

allData = rbind(dataA,dataB)


############ SVM Liniar Cross
library(e1071)
library(doParallel)
cl = makeCluster(detectCores())
registerDoParallel(cl)

(svm.model <- svm(target ~ .,data = allData, type="C-classification", kernel="linear", cross=4))

stopCluster(cl)
svmp.pred <- predict(svm.model)
table(allData$target, svmp.pred)
checkError(svmp.pred,allData$target,"SVM")


#### amb les dades del B
svmp.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svmp.predb))
checkError(svmp.predb,dataB$target,"SVM")


############ SVM Linear
library(e1071)
(svm.model <- svm(target ~ .,data = dataA, type="C-classification", kernel="linear"))
svmp.pred <- predict(svm.model)
table(dataA$target, svmp.pred)


#### amb les dades del B
svmp.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svmp.predb))



############ SVM RBF Cross
library(e1071)
library(doParallel)
library(caret)
cl = makeCluster(detectCores())
registerDoParallel(cl)
#(svm.model <- svm(target ~ .,data = allData, type="C-classification", kernel="radial", cross=4))
trc = trainControl(method="cv", number = 4, repeats = 1)
train.model = train(target ~ ., data = allData, method="svmRadial", maxit = 200, trControl = trc,
                    tuneGrid = expand.grid(C=c(0.1, 1, 10), sigma=c(0.5, 2, 10)))
stopCluster(cl)
svmp.pred <- predict(svm.model)
table(allData$target, svmp.pred)
checkError(svmp.predb,allData$target,"SVM")



#### amb les dades del B
svm.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svm.predb))

############ SVM RBF
library(e1071)
(svm.model <- svm(target ~ .,data = dataA, type="C-classification", kernel="radial"))
svmp.pred <- predict(svm.model)
table(dataA$target, svmp.pred)


#### amb les dades del B
svm.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svm.predb))
