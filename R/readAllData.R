#rm(list=ls())

normalize = function(x) {
  data.min = min(x)
  data.max = max(x)
  return((x - data.min)/(data.max - data.min))
}

readData = function(filePath, single = TRUE){
  print(filePath)
  fileName = basename(filePath)
  folderPath = sub(fileName, "", filePath)
  
  dataName = sub("_targets", "_datapoints", fileName)
  targetName = sub("_datapoints", "_targets", fileName)
  
  data = read.csv(paste0(folderPath, dataName), sep=" ", header = TRUE, colClasses = "numeric")
  data$target = unlist(read.csv(paste0(folderPath, targetName), sep=" ", header = FALSE))
  data[1:300] = apply(data[1:300], 2, scale)
  if (!single) {
    expression = sub("^[ab]_", "", sub("_datapoints.txt", "", dataName))
    data$target = lapply(unlist(data$target), function(x) {
      if (x > 0) return(expression)
      else return(as.character(x))
    })
  }
  
  # Remove the time column
  data = data[, !names(data) %in% c("X0.0")]
  return(data)
}

readAllData = function(letter = "ab", single = FALSE) {
  fileNames = choose.files(caption = "Selecciona les dades", multi=TRUE)
  fileNames = grep(paste0("[",letter, "]_.+_datapoints"), fileNames, value = TRUE)
  
  data = data.frame()
  for (i in 1:length(fileNames)) {
    aux = readData(fileNames[i], single)
    data = rbind(aux, data)
  }
  data$target = as.factor(unlist(data$target))
  return(data)
}

checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels = levels(real)), real)
  print(paste("Error", type))
  print(tab)
  
  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

