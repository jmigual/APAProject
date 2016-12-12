#####################################
## APA Laboratori 5                ##
## MLP and the RBF                 ##
## version of November, 2016       ## 
#####################################

library(MASS)
library(nnet)
set.seed (4567)

####################################################################
## Multilayer Perceptron Example 1: Admission into graduate school data
####################################################################

## Suppose we are interested in how variables, such as 

## GRE (Graduate Record Exam scores)
## GPA (Grade Point Average) and 
## rank (prestige of the undergraduate institution)

## affect admission into a graduate school.

## The target variable, admit/don't admit, is a binary variable, which we want to characterize
## and, if possible, to predict (a model)

Admis <- read.csv("Admissions.csv")

## view the first few rows of the data
head(Admis)

## We will treat all the variables gre and gpa as continuous. 
## The variable rank takes on the values 1 through 4, so we can fairly treat it as numerical
## (although, in rigour, it is ordinal)

Admis$admit <- factor(Admis$admit, labels=c("No","Yes"))

summary(Admis)

N <- nrow(Admis)

## We first split the available data into learning and test sets, selecting randomly 2/3 and 1/3 of the data
## We do this for a honest estimation of prediction performance

set.seed(43)

learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

## We start using logistic regression (a linear classifier)

model.logreg <- glm (admit~., data=Admis[learn,], family=binomial)

summary(model.logreg)

# Simplify the model using the AIC

model.logreg2 <- step(model.logreg)

# The new model has one variable less and the error (residual deviance) is virtually the same (313.5 vs 312.2)

# Interpretation of the coefficients

exp(model.logreg2$coefficients)

# Calculation of apparent error in the training set (learn)

glfpred=NULL
glfpred[model.logreg2$fitted.values<0.5]=0
glfpred[model.logreg2$fitted.values>=0.5]=1
table(Admis$admit[learn],glfpred)
error_rate.learn <- 100*(1-sum(diag(table(Admis$admit[learn],glfpred)))/nlearn)
error_rate.learn

## we get a learning error which is quite high (28.84%)

# Estimation of prediction error using the test set

glft = predict(model.logreg2, newdata=Admis[-learn,]) 
pt = 1/(1+exp(-glft))
glfpredt = NULL
glfpredt[pt<0.5]=0
glfpredt[pt>=0.5]=1
table(Admis$admit[-learn],glfpredt)
error_rate.test <- 100*(1-sum(diag(table(Admis$admit[-learn],glfpredt)))/ntest)
error_rate.test

## we get a prediction error of 27.07%

### Now we switch to non-linear modelling with a MLP

## The nnet() function is quite powerful and very reliable from the optimization
## point fo view. From the computational point of view, it has two drawbacks:

## 1- it does not have a built-in mechanism for multiple runs or cross-validation
## 2- it only admits networks of one hidden layer (of size 'size')

## Please have a look at nnet before going any further

?nnet

## The basic parameters are 'size' and 'decay' (the regularization constant, lambda)
## As usual, R detects it is a classification problem because 'admit' is a factor
## It buils a MLP with one output neuron (just two classes), with the logistic function
## and uses the cross-entropy as error function

## Let's start by scaling of inputs, this is important to avoid network 'stagnation' (premature convergence)

Admis$gpa <- scale(Admis$gpa)
Admis$gre <- scale(Admis$gre)
Admis$rank <- scale(Admis$rank)

## To illustrate the first results, we just fit a MLP with 2 hidden neurons

model.nnet <- nnet(admit ~., data = Admis, subset=learn, size=2, maxit=200, decay=0)

## Take your time to understand the output
model.nnet 

## In particular, understand why the total number of weights is 11, what 'initial  value' and 'final  value' are
## and what does 'converged' mean

# This is the final value of the error function (also known as fitting criterion)
model.nnet$value

#  fitted values for the training data
model.nnet$fitted.values

# and the residuals
model.nnet$residuals

## Now look at the weights

model.nnet$wts

## I think this way is clearer:

summary(model.nnet)

## i1,i2,i3 are the 3 inputs, h1, h2 are the two hidden neurons, b is the bias (offset)

## As you can see, some weights are large (two orders of magnitude larger then others)
## This is no good, since it makes the model unstable (i.e., small changes in some inputs may
## entail significant changes in the network, because of the large weights)

## One way to avoid this is by regularizing the learning process:

model.nnet <- nnet(admit ~., data = Admis, subset=learn, size=2, maxit=200, decay=0.01)

## notice the big difference
model.nnet$wts

summary(model.nnet)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(p1,Admis$admit[learn])
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=Admis[-learn,], type="class"))

t2 <- table(p2,Admis$admit[-learn])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## We get 26.32%, so it seems that the MLP helps a little bit; however, we need to work harder

## We are going to do the modelling in a principled way now. Using 10x10 CV to select the best
## combination of 'size' and 'decay'

## Just by curiosity, let me show you that we can fit any dataset (in the sense of reducing the training error):

model.nnet <- nnet(admit ~., data = Admis, subset=learn, size=20, maxit=200)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))

(t1 <- table(p1,Admis$admit[learn]))
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=Admis[-learn,], type="class"))

(t2 <- table(p2,Admis$admit[-learn]))
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## That's it: we got a training error around 6% (four times lower than the previous one), but it is 
## illusory ... the test error is larger than before (around 40%); 
## The relevant comparison is between 6% and 40%, this large gap is an indication of overfitting


## {caret} is an excellent package for training control, once you know what all these concepts are

## WARNING: if the package is not installed in your computer, installation needs some previous packages
library(caret)

## For a specific model, in our case the neural network, the function train() in {caret} uses a "grid" of model parameters
## and trains using a given resampling method (in our case we will be using 10x10 CV). All combinations are evaluated, and 
## the best one (according to 10x10 CV) is chosen and used to construct a final model, which is refit using the whole training set

## Thus train() returns the constructed model (exactly as a direct call to nnet() would)

## In order to find the best network architecture, we are going to explore two methods:

## a) Explore different numbers of hidden units in one hidden layer, with no regularization
## b) Fix a large number of hidden units in one hidden layer, and explore different regularization values (recommended)

## doing both (explore different numbers of hidden units AND regularization values) is usually a waste of computing 
## resources (but notice that train() would admit it)

## Let's start with a)

## set desired sizes

(sizes <- 2*seq(1,10,by=1))

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=10, repeats=10)

model.10x10CV <- train (admit ~., data = Admis, subset=learn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc)

## We can inspect the full results
model.10x10CV$results

## and the best model found
model.10x10CV$bestTune

## The results are quite disappointing ...

## Now method b)

(decays <- 10^seq(-3,0,by=0.1))

## WARNING: this takes a few minutes
model.10x10CV <- train (admit ~., data = Admis, subset=learn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=20,.decay=decays), trControl=trc)

## We can inspect the full results
model.10x10CV$results

## and the best model found
model.10x10CV$bestTune

save(model.10x10CV, file = "model.10x10CV.regul")

load ("model.10x10CV.regul")

## The results are a bit better; we should choose the model with the lowest 10x10CV error overall,
## in this case it corresponds to 20 hidden neurons, with a decay of 0.3162278

## So what remains is to predict the test set with our final model

p2 <- as.factor(predict (model.10x10CV, newdata=Admis[-learn,], type="raw"))

t2 <- table(pred=p2,truth=Admis$admit[-learn])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## We get 27.82% after all this work; it seems that the information in this dataset is not enough
## to accurately predict admittance. Note that ...

## ... upon looking at the confusion matrix for the predictions ...
t2

## it clearly suggests that quite a lot of people is getting accepted when they should not, given their gre, gpa and rank
## It is very likely that other (subjective?) factors are being taken into account, that are not in the dataset

####################################################################
## Multilayer Perceptron Example 2: circular artificial 2D data
####################################################################

set.seed(3)

p <- 2
N <- 200

x <- matrix(rnorm(N*p),ncol=p)
y <- as.numeric((x[,1]^2+x[,2]^2) > 1.4)
mydata <- data.frame(x=x,y=y)
plot(x,col=c('black','green')[y+1],pch=19,asp=1)

## Let's use one hidden layer, 3 hidden units, no regularization and the error function "cross-entropy"
## In this case it is not necessary to standardize because they variables already are
## (they have been generated from a distribution with mean 0 and standard deviation 1).

nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=3,decay=0,maxit=2000,trace=T)

yhat <- as.numeric(predict(nn1,type='class'))
par(mfrow=c(1,2))
plot(x,pch=19,col=c('red','blue')[y+1],main='actual labels',asp=1)
plot(x,col=c('red','blue')[(yhat>0.5)+1],pch=19,main='predicted labels',asp=1)
table(actual=y,predicted=predict(nn1,type='class'))

## Excellent, indeed

## Let's execute it again, this time wth a different random seed

set.seed(4)

nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=3,decay=0,maxit=2000,trace=T)
yhat <- as.numeric(predict(nn1,type='class'))
par(mfrow=c(1,2))
plot(x,pch=19,col=c('red','blue')[y+1],main='actual labels',asp=1)
plot(x,col=c('red','blue')[(yhat>0.5)+1],pch=19,main='predicted labels',asp=1)
table(actual=y,predicted=predict(nn1,type='class'))

## we see that the optimizer does not always find a good solution, even with the right number of neurons

## How many hidden units do we need?

par(mfrow=c(2,2))
for (i in 1:4)
{
  set.seed(3)
  nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=i,decay=0,maxit=2000,trace=T)
  yhat <- as.numeric(predict(nn1,type='class'))
  plot(x,pch=20,col=c('black','green')[yhat+1])
  title(main=paste(i,'hidden unit(s)'))
}


## Let's find out which function has been learned exactly, with 3 units

set.seed(3)
nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=3,decay=0,maxit=2000,trace=T)

## create a grid of values
x1grid <- seq(-3,3,l=200)
x2grid <- seq(-3,3,l=220)
xg <- expand.grid(x1grid,x2grid)
xg <- as.matrix(cbind(1,xg))

## input them to the hidden units, and get their outputs
h1 <- xg%*%matrix(coef(nn1)[1:3],ncol=1)
h2 <- xg%*%matrix(coef(nn1)[4:6],ncol=1)
h3 <- xg%*%matrix(coef(nn1)[7:9],ncol=1)

## the hidden units compute the logistic() function, so we cut the output value at 0; we get a decision line

par(mfrow=c(2,2))
contour(x1grid,x2grid,matrix(h1,200,220),levels=0)
contour(x1grid,x2grid,matrix(h2,200,220),levels=0,add=T)
contour(x1grid,x2grid,matrix(h3,200,220),levels=0,add=T)
title(main='net input = 0\n in the hidden units')

## this is the logistic function, used by nnet() for the hidden neurons, and 
## for the output neurons in two-class classification problems
logistic <- function(x) {1/(1+exp(-x))}

z <- coef(nn1)[10] + coef(nn1)[11]*logistic(h1) + coef(nn1)[12]*logistic(h2) + coef(nn1)[13]*logistic(h3)

contour(x1grid,x2grid,matrix(z,200,220))
title('hidden outputs = tanh of the net inputs\n and their weighted sum')
contour(x1grid,x2grid,matrix(logistic(z),200,220),levels=0.5)
title('logistic of the previous sum = 0.5')
contour(x1grid,x2grid,matrix(logistic(z),200,220),levels=0.5)
points(x,pch=20,col=c('black','green')[y+1])
title('same with training data points')


####################################################################
## Radial Basis Function Network Example: regression of a 1D function
####################################################################

set.seed (4)

## We are going to do all the computations "by hand"

## Let us depart from the following function in the (a,b) interval

myf <- function (x) { (1 + x - 2*x^2) * exp(-x^2) }

## We are going to model this function in the interval (-5,5)

a <- -5
b <- 5

domain <- c(a,b)

myf.data <- function (N, a, b) 
{
  x <- runif(N, a, b)
  t <- myf(x) + rnorm(N, sd=0.2)
  dd <- data.frame(x,t)
  names(dd) <- c("x", "t")
  return (dd)
}

N <- 200
d <- myf.data (N, a , b)

summary(d)

## The black points are the data, the blue line is the true underlying function

plot (d)
curve (myf, a, b, col='blue', add=TRUE)

## Create a large test data too for future use; notice that the generation mechanism is the same

N.test <- 2000
d.test <- myf.data (N.test, a , b)

# Function to compute a PHI (N x M) design matrix, without the Phi_0(x) = 1 column;
# m.i, h.i are the centers and variances (sigmas) of the neurons, respectively

PHI <- function (x,m.i,h.i)
{
  N <- length(x)
  M <- length(m.i)
  phis <- matrix(rep(0,(M)*N), nrow=M, ncol=N)
  for (i in 1:M)
  {
    phis[i,] <- exp(-(x - m.i[i])^2/(2*h.i[i]))
  }
  t(phis)
}

## We find the centers and variances for each neuron using k-means; since this clustering algorithm is non-deterministic (because the initial centers are random), we do it 'NumKmeans' times

NumKmeans <- 10

## We set a rather large number of hidden units (= basis functions) M as a function of data size (the sqrt is just a heuristic!) because we are going to try different regularizers

(M <- floor(sqrt(N)))

m <- matrix(0,nrow=NumKmeans,ncol=M)
h <- matrix(0,nrow=NumKmeans,ncol=M)

data.Kmeans <- cbind(d$x,rep(0,N))

for (j in 1:NumKmeans)
{
  # Find the centers m.i with k-means
  km.res <- cclust (x=data.Kmeans, centers=M, iter.max=200, method="kmeans",dist="euclidean")
  m[j,] <- km.res$centers[,1]
  
  # Obtain the variances h.i as a function of the m.i
  h[j,] <- rep(0,M)
  for (i in 1:M)
  {
    indexes <- which(km.res$cluster == i)
    h[j,i] <- sum(abs(d$x[indexes] - m[j,i]))/length(indexes)
    if (h[j,i] == 0) h[j,i] <- 1
  }
}


## Now for each k-means we get the hidden-to-output weights by solving a regularized
## least-squares problem (standard ridge regression), very much as we did in previous labs

## The difference is that now we perform ridge regression on the PHI matrix (that is, on the new regressors given by the hidden neurons), not on the original inputs ...

## ... and find the best lambda with using GCV across all choices of basis functions (the NumKmeans clusterings)

(lambdes <- 10^seq(-3,1.5,by=0.1))

library(MASS) # we need it for lm.ridge

errors <- rep(0,NumKmeans)
bestLambdes <- rep(0,NumKmeans)

# For each k-means result
for (num in 1:NumKmeans)
{
  m.i <- m[num,]
  h.i <- h[num,]
  
  myPHI <- PHI (d$x,m.i,h.i)
  aux1 <- lm.ridge(d$t ~ myPHI, d, lambda = lambdes)
  my.lambda <- as.numeric(names(which.min(aux1$GCV)))
  
  aux2 <- lm.ridge(d$t ~ myPHI, d, lambda = my.lambda)
  
  errors[num] <- sqrt(aux2$GCV)
  bestLambdes[num] <- my.lambda
  
}


## Now we obtain the best model among the tested ones

bestIndex <- which(errors == min(errors))
bestLambda <- bestLambdes[bestIndex]
m.i <- m[bestIndex,]
h.i <- h[bestIndex,]

## we see that this problem needs a lot of regularization! This makes sense if you take a look at how the data is generated (the previous plot): the noise level is very high relative to the signal

## We also see that the best lambda fluctuates (since the data changes  due to the clustering, but the order of magnitude is quite stable

bestLambdes

## We now create the final model:

my.RBF <- lm.ridge(d$t ~ PHI (d$x,m.i,h.i), d, lambda = bestLambda)

## these are the final hidden-to-output weights: note how small they are (here is where we regularize)
(w.i <- setNames(coef(my.RBF), paste0("w_", 0:M)))

## It remains to calculate the prediction on the test data

test.PHI <- cbind(rep(1,length(d.test$x)),PHI(d.test$x,m.i,h.i))
y <- test.PHI %*% w.i

## And now the normalized error of this prediction

(errorsTest <- sqrt(sum((d.test$t - y)^2)/((N.test-1)*var(d.test$t))))

## Much better if we plot everything

par(mfrow=c(1,1))

## Test data in black
plot(d.test$x,d.test$t,xlab="x",ylab="t",main=paste("Prediction (learning size: ",toString(N),"examples)"),ylim=c(-1.5,1.5))

## Red data are the predictions
points(d.test$x,y,col='red',lwd=1)

## and the blue line is the underlying function
curve (myf, a, b, col='blue', add=TRUE)


## The previous code is designed for 1D problems but you can easily adapt it to more input dimensions

## There is a general package for neural networks: {RSNNS}

## Which is actually the R interface to the (formerly widely) used and flexible Stuttgart Neural Network Simulator (SNNS)

## This library contains many standard implementations of neural networks. The {RSNNS} package actually wraps the SNNS functionality to make it available from within R

## The RBF version within this package has a sophisticated method for initializing the networt, which is also quite non-standard, so we avoid further explanation

## Sadly this package does not provide with a way to control regularization or to allow for multi-class problems (a softmax option with the cross-entropy would be welcome