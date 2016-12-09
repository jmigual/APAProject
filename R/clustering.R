rm(list=ls())
library(MASS)

plotPosNeg <- function(posX, negX, posY, negY, ...) {
  X = c(posX, negX)
  Y = c(posY, negY)
  meanX = mean(X)
  meanY = mean(Y)
  plot(posX - meanX, posY - meanY, col='green', ylim=c(min(Y),max(Y)), xlim=c(min(X),max(X)), ...)
  points(negX - meanX, negY - meanY, col='red')
}

data = read.csv(choose.files(caption="Selecciona dades", multi=FALSE), sep=" ", header = TRUE)
data$target = read.csv(choose.files(caption="Selecciona resposta", multi=FALSE), sep=" ", header = FALSE)

# Punt a ser projectat
point = 60

columns = 2 + (point*3):(point*3 + 2)
pData = data[columns]

# Seleccionar punts positius i negatius
pData$target = data$target
positive = subset(pData, target == 1)
negative = subset(pData, target == 0)

# Crida a LDA
myLDA = lda(pData[1:3], unlist(data$target))

# Imprimir resultats LDA
plotPosNeg(positive[,1], negative[,1], positive[,2], negative[,2], 
           main="Direction for projection using FDA", xlab="X", ylab="Y")
abline(0, myLDA$scaling[2]/myLDA$scaling[1], col='black',lwd=2)
plot(c(positive[,1],negative[,1]) - mean(pData[,1]), c(positive[,2],negative[,2]) - mean(pData[,2]), 
     col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main="Direction for projection using FDA", xlab="X", ylab="Y")
abline(0, myLDA$scaling[2]/myLDA$scaling[1], col='black',lwd=2)

plot(c(positive[,1],negative[,1]) - mean(pData[,1]), c(positive[,3],negative[,3]) - mean(pData[,3]), 
     col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main="Direction for projection using FDA", xlab="X", ylab="Z")
abline(0, myLDA$scaling[3]/myLDA$scaling[1], col='black',lwd=2)

plot(c(positive[,2],negative[,2]) - mean(pData[,2]), c(positive[,3],negative[,3]) - mean(pData[,3]), 
     col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main="Direction for projection using FDA", xlab="Y", ylab="Z")
abline(0, myLDA$scaling[3]/myLDA$scaling[2], col='black',lwd=2)

posproj = positive[,1]*myLDA$scaling[1] + positive[,2]*myLDA$scaling[2] + positive[,3]*myLDA$scaling[3]
negproj = negative[,1]*myLDA$scaling[1] + negative[,2]*myLDA$scaling[2] + negative[,3]*myLDA$scaling[3]
plot(c(posproj, negproj), rep(0,nrow(pData)), col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main='FDA projection as seen in 1D', xlab="Discriminant", ylab="")

# Ara calculem el PCA
myPCA <- prcomp(pData[c(1,2,3)],scale=TRUE)
plot(c(positive[,1],negative[,1]) - mean(pData[,1]), c(positive[,2],negative[,2]) - mean(pData[,2]), 
     col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main="Direction for projection using PCA", xlab="X", ylab="Y")
abline(0, myPCA$rotation[2,1]/myPCA$rotation[1,1],col='black',lwd=2)

plot(c(positive[,1],negative[,1]) - mean(pData[,1]), c(positive[,3],negative[,3]) - mean(pData[,3]), 
     col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main="Direction for projection using PCA", xlab="X", ylab="Z")
abline(0, myPCA$rotation[3,1]/myPCA$rotation[1,1], col='black',lwd=2)

plot(c(positive[,2],negative[,2]) - mean(pData[,2]), c(positive[,3],negative[,3]) - mean(pData[,3]), 
     col=c(rep('green',nrow(positive)),rep('red',nrow(negative))),
     main="Direction for projection using PCA", xlab="Y", ylab="Z")
abline(0, myPCA$rotation[3,1]/myPCA$rotation[2,1], col='black',lwd=2)

