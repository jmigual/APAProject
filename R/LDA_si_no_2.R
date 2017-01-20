# Eliminar totes les dades anteriors
rm(list=ls())
library(MASS)
source("./R/readAllData.R")
source("./R/reduceData.R")

#######################
# INICI DE L'EXECUCIO #
#######################

allData = reduceData(readAllData(single = TRUE))

# Donat que pel model nomes hi ha dues persones que aporten dades
# es barregen totes les dades i s'agafen algunes aleatòries per 
# entrenar el model, després es valida amb la resta de dades

index.sample = sample(nrow(allData), nrow(allData)/7)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]


lda.res = lda(target ~ ., data = data.train)

# Visualitzacio de l'error de training
pred.train = predict(lda.res)
checkError(pred.train$class, data.train$target, "training")

# Visualitzacio de l'error de testing
pred.valid = predict(lda.res, newdata = data.valid)
checkError(pred.valid$class, data.valid$target, "validacio")
