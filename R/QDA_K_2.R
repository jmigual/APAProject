# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
source("./R/readAllData.R")

#######################
# INICI DE L'EXECUCIO #
#######################
allData = readAllData()

index.sample = sample(nrow(allData), nrow(allData)/2)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]

##Generem el model
qda.model <- qda (data.train$target ~ ., data.train)

qda.model

# Visualitzacio de l'error de training
pred.train = predict(qda.model)
checkError(pred.train$class, data.train$target, "training")

# Visualitzacio de l'error de testing
pred.valid = predict(qda.model, newdata = data.valid)
checkError(pred.valid$class, data.valid$target, "validacio")

