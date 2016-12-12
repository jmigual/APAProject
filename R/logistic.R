# Elmininar totes les dades anteriors
rm(list=ls())
library(MASS)

# Aquest script realitza regressio logistica amb les dades obtingudes per a una sola expressio facial amb dos conjunts de dades,
# Per entrenar es pot utilitzar la persona A i per provar les dades, la persona B

readData = function(name){
  filePath = choose.files(caption = paste("Selecciona dades", name), multi=FALSE)
  fileName = basename(filePath)
  folderPath = sub(fileName, "", filePath)
  
  dataName = sub("_target", "_datapoints", fileName)
  targetName = sub("_datapoints", "_targets", fileName)
  
  data = read.csv(paste0(folderPath, dataName), sep=" ", header = TRUE)
  targetData = read.csv(paste0(folderPath, targetName), sep=" ", header = FALSE)
  
  targetFactors = as.factor(unlist(targetData[[1]]))
  data$target = targetFactors
  
  # Remove the time column
  return(data[, !names(data) %in% c("X0.0")])
}

checkValue = function(x) {
  if (x > 0) return(1)
  else return(0)
}

trainAndError = function(data, dataT, family = quasibinomial, maxit = 50) {
  # La columna target es la que es vol obtenir per regressio logistica
  glm.res = glm(target ~ ., data, family = family, control = list(maxit = maxit))
  
  # Check the training error
  probability = exp(predict(glm.res, dataT))
  
  # Obtenir els factors
  value = as.factor(sapply(probability, checkValue))
  
  # Obtenir el % d'error
  table = table(value, dataT[,301])
  print(table)
  
  error = 100 - (sum(diag(table)))/length(value) * 100
  print(error)
}

# Llegir les dades training
data = readData("training")

# Llegir dades testing
dataT = readData("testing")


# Regressio utilitzant un model binomial
trainAndError(data, dataT, "quasibinomial")