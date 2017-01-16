# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
source("./R/readAllData.R")
source("./R/reduceData.R")

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

allData = reduceData(readAllData(single = TRUE))

# Donat que pel model nomes hi ha dues persones que aporten dades
# es barregen totes les dades i s'agafen algunes aleatòries per 
# entrenar el model, després es valida amb la resta de dades

index.sample = sample(nrow(allData), nrow(allData)/4)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]


lda.res = lda(target ~ ., data = data.train)

# Visualitzacio de l'error de training
pred.train = predict(lda.res)
checkError(pred.train$class, data.train$target, "training")

# Visualitzacio de l'error de testing
pred.valid = predict(lda.res, newdata = data.valid)
checkError(pred.valid$class, data.valid$target, "validacio")
