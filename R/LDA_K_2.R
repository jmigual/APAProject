# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
source("./R/readAllData.R")

#######################
# INICI DE L'EXECUCIO #
#######################

# Llegir les dades
allData = readAllData()

index.sample = sample(nrow(allData), nrow(allData)/7)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]

# Generem el model
lda.model <- lda (data.train$target ~ ., data.train)
lda.model

# Visualitzacio de l'error de training
pred.train = predict(lda.model)
checkError(pred.train$class, data.train$target, "training")

#Error de cross validation
pred.cv <- update(lda.model,CV=TRUE)
checkError(pred.cv$class, data.train$target, "cross")

# Visualitzacio de l'error de testing
pred.valid = predict(lda.model, newdata = data.valid)
checkError(pred.valid$class, data.valid$target, "validacio")
