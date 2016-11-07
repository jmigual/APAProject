rm(list=ls())
data <- read.csv(file.choose(), sep=" ")

cols = (ncol(data) - 1)/3

Xcols = (1:cols)*3 - 1
Ycols = 1 + Xcols
Zcols = 1 + Ycols

Tpoints = data[1]
Xpoints = data[Xcols]
Ypoints = -1 * data[Ycols]
Zpoints = data[Zcols]

Xcoord = stack(Xpoints)
Ycoord = stack(Ypoints)

pData = Xcoord
pData$Yval = Ycoord$values
pData$time = rep(1:nrow(data), ncol(Xpoints))
pData = pData[,c(4,2,1,3)]
names(pData)[2:3] = c("id", "XVal")

library(googleVis)
chartData <- gvisMotionChart(pData, idvar="id", timevar="time")
plot(chartData)
#h1 = chartData$html
#htmlstring <- paste(h1, collapse = "\n")
#cat(htmlstring, file="index.html")




