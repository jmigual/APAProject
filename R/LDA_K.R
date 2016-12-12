# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
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

##Generem el model
lda.model <- lda (dataA$target ~ . , dataA)

lda.model

# Visualitzacio de l'error de training
pred.train = predict(lda.model)
checkError(pred.train$class, dataA$target, "training")

#Error de cross validation
pred.cv <- update(lda.model,CV=TRUE)
checkError(pred.cv$class, dataA$target, "cross")

# Visualitzacio de l'error de testing
pred.valid = predict(lda.model, newdata = dataB)
checkError(pred.valid$class, dataB$target, "validacio")
