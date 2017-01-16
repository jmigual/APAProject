rm(list = ls())
library(MASS)
library(nnet)
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

# Per comprovar les dades s'usará K-Cross Validation


trc <- trainControl (method="cv", number=10, repeats=2)

cl = makeCluster(detectCores())
registerDoParallel(cl)

# Best option is size = 5, decay = 0.1
model.train = train (target ~., data = allData, method='nnet', maxit = 200, trControl=trc, MaxNWts = 10000,
                     tuneGrid = expand.grid(.size=seq(3,15,by = 2),.decay=c(0, 0.01, 0.1)))
stopCluster(cl)

train.car.pred = predict(model.train)
checkError(train.car.pred, allData$target, "Train result")

print(model.train$bestTune)

# Un cop obinguts els paràmetres mitjançant el train ja només cal entrenar la xarxa neuronal
# que té els millors paràmetres.
model.nnet = nnet(target ~ ., data = allData, size = 5, decay = 0.1, maxit = 500, MaxNWts = 100000)

train.pred = predict(model.nnet, type = "class")
checkError(train.pred, allData$target, "train")

