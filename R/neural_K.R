rm(list = ls())
library(MASS)
library(nnet)
library(caret)
library(doParallel)

source("./R/readAllData.R")
source("./R/reduceData.R")

#######################
# INICI DE L'EXECUCIO #
#######################

# Llegir les dades de training
allData = reduceData(readAllData())

# Donat que pel model nomes hi ha dues persones que aporten dades
# es barregen totes les dades i s'agafen algunes aleatòries per 
# entrenar el model, després es valida amb la resta de dades
index.sample = sample(nrow(allData), nrow(allData)/7)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]


# Per comprovar les dades de la rutina train s'usará K-Cross Validation
trc <- trainControl (method="cv", number=4, repeats=1)

cl = makeCluster(detectCores())
registerDoParallel(cl)

# Best option is size = 19, decay = 0.1
model.train = train (target ~., data = data.train, method='nnet', maxit = 200, 
                     trControl=trc, MaxNWts = 10000, tuneGrid = 
                     expand.grid(.size=seq(13,21,by = 2),.decay=c(0, 0.01, 0.1)))
stopCluster(cl)

train.car.pred = predict(model.train)
checkError(train.car.pred, data.train$target, "best train with training")

train.car.valid = predict(model.train, newdata = data.valid)
checkError(train.car.valid, data.valid$target, "best train with validation")

print(model.train$bestTune)

# Un cop obinguts els paràmetres mitjançant el train ja només cal entrenar la xarxa neuronal
# que té els millors paràmetres.
model.nnet = nnet(target ~ ., data = data.train, size = 19, decay = 0.1, maxit = 500, MaxNWts = 100000)

train.pred = predict(model.nnet, type = "class")
checkError(train.pred, data.train$target, "train")

train.valid = predict(model.nnet, type = "class", newdata = data.valid)
checkError(train.valid, data.valid$target, "validation")
