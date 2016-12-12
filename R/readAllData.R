rm(list=ls())

readData = function(filePath){
  print(filePath)
  fileName = basename(filePath)
  folderPath = sub(fileName, "", filePath)
  
  dataName = sub("_targets", "_datapoints", fileName)
  targetName = sub("_datapoints", "_targets", fileName)
  
  data = read.csv(paste0(folderPath, dataName), sep=" ", header = TRUE)
  targetData = read.csv(paste0(folderPath, targetName), sep=" ", header = FALSE)
  
  targetFactors = as.factor(unlist(targetData[[1]]))
  data$target = targetFactors
  
  # Remove the time column
  return(data[, !names(data) %in% c("X0.0")])
}

readAllData = function(letter) {
  fileNames = choose.files(caption = "Selecciona les dades", multi=TRUE)
  fileNames = grep(paste0(letter, "_.+_datapoints"), fileNames, value = TRUE)
  
  data = data.frame()
  for (i in 1:length(fileNames)) {
    aux = readData(fileNames[i])
    aux$target <- as.numeric(aux$target) * i
    if (i == 1) {
      data = aux
      print(nrow(data))
    } else {
      data = rbind(data, aux)
      print(nrow(data))
    }
  }
  return(data)
}

data = readAllData("a")
