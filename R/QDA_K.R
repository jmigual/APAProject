# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
source("./R/readAllData.R")

checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels=0:1), real)
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

##Generem el model
qda.model <- qda (dataA$target ~ ., dataA)

qda.model

# Visualitzacio de l'error de training
pred.train = predict(qda.model)
checkError(pred.train$class, dataA$target, "training")

#Error de cross validation
pred.cv <- update(qda.model,CV=TRUE)
checkError(pred.cv$class, dataA$target, "cross")

# Visualitzacio de l'error de testing
pred.valid = predict(qda.model, newdata = dataB)
checkError(pred.valid$class, dataB$target, "validacio")