rm(list=ls())
library(MASS)

############rm(list=ls())

readData = function(filePath, single = TRUE){
  print(filePath)
  fileName = basename(filePath)
  folderPath = sub(fileName, "", filePath)
  
  dataName = sub("_targets", "_datapoints", fileName)
  targetName = sub("_datapoints", "_targets", fileName)
  
  data = read.csv(paste0(folderPath, dataName), sep=" ", header = TRUE, colClasses = "numeric")
  data$target = unlist(read.csv(paste0(folderPath, targetName), sep=" ", header = FALSE))
  if (!single) {
    expression = sub("^[ab]_", "", sub("_datapoints.txt", "", dataName))
    data$target = lapply(unlist(data$target), function(x) {
      if (x > 0) return(expression)
      else return(as.character(x))
    })
  }
  
  # Remove the time column
  return(data[, !names(data) %in% c("X0.0")])
}

readAllData = function(letter, single = FALSE) {
  fileNames = choose.files(caption = "Selecciona les dades", multi=TRUE)
  fileNames = grep(paste0(letter, "_.+_datapoints"), fileNames, value = TRUE)
  
  data = data.frame()
  for (i in 1:length(fileNames)) {
    aux = readData(fileNames[i], single)
    data = rbind(aux, data)
  }
  data$target = as.factor(unlist(data$target))
  return(data)
}

dataA = readAllData("a")
dataB = readAllData("b")
############


lda.model <- lda (dataA$target ~ . , dataA)

lda.model

plot(lda.model)

ldap.predcv <- update(lda.model,CV=TRUE)
head(ldap.predcv$posterior)
print(table(dataA$target,ldap.predcv$class))

z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
predb <- predict(dataB, lda.model)$class

############

qda.model <- qda (dataA$target ~ ., dataA)

qda.model

qdap.pred <- predict(qda.model)
table(dataA$target, qdap.pred$class)


qdap.predcv <- update(qda.model,CV=TRUE)

head(qdap.predcv$posterior)
table(dataA$target, qdap.predcv$class)

#############