rm(list=ls())
library(MASS)
source("./R/readAllData.R")

plotPosNeg <- function(posX, negX, posY, negY, ...) {
  X = c(posX, negX)
  Y = c(posY, negY)
  meanX = mean(X)
  meanY = mean(Y)
  plot(posX - meanX, posY - meanY, col='green', ylim=c(min(Y),max(Y)) - meanY, xlim=c(min(X),max(X)) - meanX, ...)
  points(negX - meanX, negY - meanY, col='red')
}

# data = read.csv(choose.files(caption="Selecciona dades", multi=FALSE), sep=" ", header = TRUE)
# data$target = read.csv(choose.files(caption="Selecciona resposta", multi=FALSE), sep=" ", header = FALSE)

data = readData(file.choose(), single = TRUE)

# Punt a ser projectat
point = 1

columns = (point*3):(point*3 + 2)
pData = data[columns]
pData$target = data$target

# Seleccionar punts positius i negatius
positive = subset(pData, target == 1)
negative = subset(pData, target == 0)

# Crida a LDA
myLDA = lda(pData[1:3], unlist(data$target))

# Imprimir resultats LDA
plotPosNeg(positive[,1], negative[,1], positive[,2], negative[,2], 
           main="Direction for projection using FDA", xlab="X", ylab="Y")
abline(0, myLDA$scaling[2]/myLDA$scaling[1], col='black',lwd=2)

plotPosNeg(positive[,1], negative[,1], positive[,3], negative[,3],
           main="Direction for projection using FDA", xlab="X", ylab="Z")
abline(0, myLDA$scaling[3]/myLDA$scaling[1], col='black',lwd=2)

plotPosNeg(positive[,2], negative[,2], positive[,3], negative[,3], 
           main="Direction for projection using FDA", xlab="Y", ylab="Z")
abline(0, myLDA$scaling[3]/myLDA$scaling[2], col='black',lwd=2)

posproj = positive[,1]*myLDA$scaling[1] + positive[,2]*myLDA$scaling[2] + positive[,3]*myLDA$scaling[3]
negproj = negative[,1]*myLDA$scaling[1] + negative[,2]*myLDA$scaling[2] + negative[,3]*myLDA$scaling[3]
plot(c(posproj, negproj), rep(0,nrow(pData)), col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main='FDA projection as seen in 1D', xlab="Discriminant", ylab="")

plot(myLDA)

# Ara calculem el PCA
myPCA <- prcomp(pData[c(1,2,3)],scale=TRUE)
plotPosNeg(positive[,1], negative[,1], positive[,2], negative[,2], 
           main="Direction for projection using PCA", xlab="X", ylab="Y")
abline(0, myPCA$rotation[2,1]/myPCA$rotation[1,1],col='black',lwd=2)

plotPosNeg(positive[,1], negative[,1], positive[,3], negative[,3], 
           main="Direction for projection using PCA", xlab="X", ylab="Z")
abline(0, myPCA$rotation[3,1]/myPCA$rotation[1,1], col='black',lwd=2)

plotPosNeg(positive[,2], negative[,2], positive[,3], negative[,3], 
           main="Direction for projection using PCA", xlab="Y", ylab="Z")
abline(0, myPCA$rotation[3,1]/myPCA$rotation[2,1], col='black',lwd=2)

