rm(list = ls())
library(MASS)
source("./R/readAllData.R")

reduceData = function(data) {
  result = data.frame(X0 = 1:nrow(data))
  for (i in seq(1, ncol(data) - 1, 3)) {
    myLDA = lda(data[i:(i+2)], unlist(data$target))
    scale = myLDA$scaling
    projection = data[,i]*scale[1] + data[,i + 1]*scale[2] + data[,i + 2]*scale[3]
    result = cbind(result, projection)
  }
  result = cbind(result[2:ncol(result)], data[ncol(data)])
  colnames(result) = c(sapply(1:(ncol(result) - 1), function(x){paste0("x",x)}), "target")
  return(result)
}

# This is for testing purposes only
#dataA = readAllData("a")
#dataB = readAllData("b")
#allData = rbind(dataA, dataB)
#res = reduceData(allData)
