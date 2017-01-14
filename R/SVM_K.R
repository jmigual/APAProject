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

(svm.model <- svm(target ~ .,data = allData, type="C-classification", kernel="linear", cross=2))

stopCluster(cl)
svmp.pred <- predict(svm.model)
table(dataA$target, svmp.pred)


#### amb les dades del B
svmp.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svmp.predb))

############ SVM Linear
library(e1071)
(svm.model <- svm(target ~ .,data = dataA, type="C-classification", kernel="linear"))
svmp.pred <- predict(svm.model)
table(dataA$target, svmp.pred)


#### amb les dades del B
svmp.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svmp.predb))

############ SVM RBF
library(e1071)
(svm.model <- svm(target ~ .,data = dataA, type="C-classification", kernel="radia"))
svmp.pred <- predict(svm.model)
table(dataA$target, svmp.pred)


#### amb les dades del B
svm.predb <- predict(svm.model, newdata = dataB)
print(table(dataB$target,svm.predb))
