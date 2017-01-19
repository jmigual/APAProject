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

#######################
# INICI DE L'EXECUCIO #
#######################

# Llegir les dades de training
allData = reduceData(readAllData( single = FALSE))

# Donat que pel model nomes hi ha dues persones que aporten dades
# es barregen totes les dades i s'agafen algunes aleatòries per 
# entrenar el model, després es valida amb la resta de dades
index.sample = sample(nrow(allData), nrow(allData)/7)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]

# Per comprovar les dades s'usará K-Cross Validation

trc = trainControl(method="cv", number = 4, repeats = 2)

cl = makeCluster(detectCores())
registerDoParallel(cl)

#The final values used for the model were sigma = 0.05 and C = 16. 
svm.model = train(target ~ ., data = data.train, method="svmRadial", maxit = 200, trControl = trc,
                    tuneGrid = expand.grid(C=c(15, 16, 17), sigma=c(0.01, 0.05, 0.1)))
stopCluster(cl)
svm.pred <- predict(svm.model)
table(data.train$target, svm.pred)
checkError(svm.pred,data.train$target,"SVM train")

#### amb totes les de validacio
svm.pred <- predict(svm.model, newdata = data.valid)
print(table(data.valid$target,svm.pred))
checkError(svm.pred,data.valid$target,"SVM test")


####### Linear
# Per comprovar les dades s'usará K-Cross Validation

trc = trainControl(method="cv", number = 10, repeats = 2)

cl = makeCluster(detectCores())
registerDoParallel(cl)


svm.model = train(target ~ ., data = data.train, method="svmLinear", maxit = 200, trControl = trc,
                  tuneGrid = expand.grid(C=c(8,9,10)))
stopCluster(cl)
svm.pred <- predict(svm.model)
table(data.train$target, svm.pred)
checkError(svm.pred,data.train$target,"SVM train")

#### amb totes les de validacio
svm.pred <- predict(svm.model, newdata = data.valid)
print(table(data.valid$target,svm.pred))
checkError(svm.pred,data.valid$target,"SVM test")

