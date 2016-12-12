# Elmininar totes les dades anteriors
rm(list=ls())
source("./R/readAllData.R")

# Aquest script realitza regressio logistica amb les dades obtingudes per a una sola expressio facial amb dos conjunts de dades,
# Per entrenar es pot utilitzar la persona A i per provar les dades, la persona B
probValue = function(x) {
  if (x > 0) return(1)
  else return(0)
}

checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels=0:1), real)
  print(paste("Error", type))
  print(tab)
  
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

trainAndError = function(data.train, data.test, family = quasibinomial, maxit = 50) {
  # La columna target es la que es vol obtenir per regressio logistica
  glm.res = glm(target ~ ., data.train, family = family, control = list(maxit = maxit))
  
  # Obtenir el % d'error amb el conjunt de training
  prob.train = predict(glm.res, newdata = data.train)
  checkError(lapply(prob.train, probValue), data.train[,301], "training")
  
  # Obtenir el % d'error amb el conjunt de testing
  prob.test = predict(glm.res, newdata = data.test)
  checkError(lapply(prob.test, probValue), data.test[,301], "testing")
}

#######################
# INICI DE L'EXECUCIO #
#######################
# Llegir les dades training
data.train = readAllData("a", single = TRUE)

# Llegir dades testing
data.test = readAllData("b", single = TRUE)


# Regressio utilitzant un model binomial
trainAndError(data.train, data.test, "quasibinomial")
