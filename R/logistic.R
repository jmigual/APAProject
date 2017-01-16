# Elmininar totes les dades anteriors
rm(list=ls())
source("./R/readAllData.R")
source("./R/reduceData.R")

# Aquest script realitza regressio logistica amb les dades obtingudes per a una sola expressio facial amb dos conjunts de dades,
# Per entrenar es pot utilitzar la persona A i per provar les dades, la persona B
probValue = function(x) {
  if (x > 0) return(1)
  else return(0)
}

######################
# INICI DE L'EXECUCIO #
#######################

allData = reduceData(readAllData(single = TRUE))

# Donat que pel model nomes hi ha dues persones que aporten dades
# es barregen totes les dades i s'agafen algunes aleatòries per 
# entrenar el model, després es valida amb la resta de dades
index.sample = sample(nrow(allData), nrow(allData)/7)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]

# Regressio utilitzant un model binomial
glm.res = glm(target ~ ., data.train, family = "quasibinomial", control = list(maxit = 200))

# Obtenir el % d'error amb el conjunt de training
prob.train = predict(glm.res)
checkError(lapply(prob.train, probValue), data.train$target, "training")

# Obtenir el % d'error amb el conjunt de validacio
prob.valid = predict(glm.res, newdata = data.valid)
checkError(lapply(prob.valid, probValue), data.valid$target, "validacio")

