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
data.train = readAllData("a", single = TRUE)

# Llegir les dades de testing
data.test = readAllData("b", single = TRUE)

lda.res = lda(target ~ ., data = data.train)

# Visualitzacio de l'error de training
pred.train = predict(lda.res)
checkError(pred.train$class, data.train$target, "training")

# Visualitzacio de l'error de testing
pred.test = predict(lda.res, newdata = data.test)
checkError(pred.test$class, data.test$target, "testing")
