# Elmininar totes les dades anteriors
rm(list=ls())
source("./R/readAllData.R")

# Aquest script realitza regressio logistica amb les dades obtingudes per a una sola expressio facial amb dos conjunts de dades,
# Per entrenar es pot utilitzar la persona A i per provar les dades, la persona B
probValue = function(x) {
  str(x)
  return (x)
  return(which.max(x))
  if (x > 0) return(1)
  else return(0)
}

checkError = function(predicted, real, type) {
  tab = table(predicted, real)
  print(paste("Error", type))
  print(tab)
  
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

trainAndError = function(data.train, data.valid, family = quasibinomial, maxit = 50) {
  # La columna target es la que es vol obtenir per regressio logistica
  glm.res = glm(target ~ ., data.train, family = family, control = list(maxit = maxit))
  
  #confint(glm.res)
  
  # Odds ratio and 95% CI
  #exp(cbind(OR = coef(glm.res), confint(glm.res)))
  
  
  # Obtenir el % d'error amb el conjunt de training
  prob.train = predict(glm.res, newdata = data.train, type='response')
  #print(print(str((prob.train))))
  #view(prob.train)
  
  checkError(lapply(prob.train, probValue), data.train[,301], "training")
  
  # Obtenir el % d'error amb el conjunt de validacio
  prob.valid = predict(glm.res, newdata = data.valid)
  prob.validw = probValue(prob.valid)
  #view(prob.valid)
  checkError(prob.validw, data.valid[,301], "validacio")
}

#######################
# INICI DE L'EXECUCIO #
#######################
# Llegir les dades training

allData = readAllData()

index.sample = sample(nrow(allData), nrow(allData)/7)
data.train = allData[index.sample,]
data.valid = allData[!1:nrow(allData) %in% index.sample,]

#data.train = readAllData("a", single = FALSE)

# Llegir dades testing
#data.valid = readAllData("b", single = FALSE)


# Regressio utilitzant un model binomial
trainAndError(data.train, data.valid, "quasibinomial")

