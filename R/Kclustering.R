rm(list=ls())
library(MASS)



lda.model <- lda (data.target ~ .)

lda.model

plot(lda.model)


qda.model <- qda (data.target ~ .)

qda.model