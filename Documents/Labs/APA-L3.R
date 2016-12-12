######################################
## APA Laboratori 3                 ##
## Sistemes lineals (regularitzats) ##
## version of October, 2016         ## 
######################################

#############################################
# Escalfament ...
#############################################

# veiem que les matrius A·A^T i A^T·A són diferents

# definim 'i'
i <- sqrt(as.complex(-1))
(A <- matrix(c(0,i,0,1),nrow=2,byrow = TRUE))

(A %*% t(A))  # A·A^T és la matriu (-1 i; i 1)

(t(A) %*% A)  # A^T·A és la matriu tot zeros

#############################################
# EXEMPLE 1
# Resolució d'un problema artificial senzill
#############################################


# Definició del sistema lineal a resoldre
## t = f(x) + epsilon
## on f(x) = (1 + 1/9)(x-1) + 10 i epsilon ~ N(0,1)

N <- 10

X <- matrix(c(rep(1,N), seq(N)),nrow=N)

X

t <- seq(10,20,length.out=N) + rnorm(N)

plot(X[,2],t,lwd=3)


#############################################
# Solució de problemes de mínims quadrats de l'estil 
#       min_w || t - Xw ||^2

# 1. Resolució mitjançant la pseudo-inversa

(C <- t(X) %*% X)  # X^T X

(X.pseudo <- solve(C) %*% t(X))  # (X^T X)^{-1} X^T

(X.pseudo %*% X) # és pseudo-inversa esquerra d'X

(w <- X.pseudo %*% t) # solució del problema

lines (X[,2], w[2,1]*X[,2]+w[1,1], type="l")

# 2. Resolució mitjançant la SVD

(s <- svd(X))

# Les dues columnes d'X són linealment independents, i per tant els dos valors singulars són diferents de 0; en altres paraules, rang(X) = 2 = min(10,2), per tant X és "full rank"

# ara comprovem que X = U D V^T

D <- diag(s$d)
s$u %*% D %*% t(s$v)

# Ara ho apliquem a la solució del problema de mínims quadrats
#       min_w || t - Xw ||^2

D <- diag(1/s$d)
(w <- s$v %*% D %*% t(s$u) %*% t)

# noteu que obtenim la mateixa solució

## La rutina glm() implementa regressió lineal per mínims quadrats si li demanem
## mitjançant el paràmetre family = gaussian

(mostra <- data.frame(x=X,t=t))

# Noteu que glm sempre afegeix un terme "intercept" ó "offset" (un regressor constant 1) per defecte, així que tenim dues opcions:

# 1. desactivar-lo (el "-1" a la fòrmula següent) i usar la nostra pròpia columna de 1's
model1 <- glm (t ~ x.2 + x.1 - 1, data=mostra, family = gaussian)

# 2. usar la que glm ja posa (recomanat) i desactivar la nostra pròpia columna de 1's
model2 <- glm (t ~ x.2, data=mostra, family = gaussian)

# Els coefficients (el vector w)

model1$coefficients
model2$coefficients

#############################################
# Per què la SVD?
#############################################

# a. En formar la matriu X^T X es pot perdre informació

eps <- 1e-3
(X.eps <- matrix(c(1,eps,0,1,0,eps),nrow=3))

((C.eps <- t(X.eps) %*% X.eps))  

solve(C.eps) # comencem a tenir problemes ...

eps <- 1e-10
(X.eps <- matrix(c(1,eps,0,1,0,eps),nrow=3))

(C.eps <- t(X.eps) %*% X.eps)

solve(C.eps) # dóna error (la matriu 2x2 "tot uns" és singular)

# (el determinant és 1·1 - 1·1 = 0)

# però no ho hauria de ser ... aquesta no és la nostra matriu ...
# el problema és que l'operacio X^T X fa perdre molta precissió numèrica (hem perdut epsilon)


# b. El número de condició d'una matriu

# El número de condició d'una matriu és el producte entre la norma de la matriu
# i la norma de la seva inversa
# Dóna una indicació de l'exactitud dels resultats de la inversió d'una matriu
# Valors prop d'1 indiquen una matriu ben condicionada

# El número de condició de la matriu X^T X és el quadrat del de la matriu X

# Incidentalment, rl número de condició corresponent a usar la norma-2 equival al quocient entre el valor singular més gran i el més petit (no nul) de la matriu

# La rutina kappa() calcula el número de condició

kappa(X, exact=TRUE)

kappa(t(X) %*% X, exact=TRUE)

# veiem-ho amb un exemple:

(X <- matrix(c(rep(1,N), 100+seq(N)),nrow=N))

kappa(X, exact=TRUE)

kappa(t(X) %*% X, exact=TRUE)

# Una solució molt senzilla és centrar la segona columna:

(X <- matrix(c(rep(1,N), 100+seq(N)),nrow=N))

X[,2] <- X[,2] - mean(X[,2])

X

kappa(X, exact=TRUE)

kappa(t(X) %*% X, exact=TRUE)

# Hi ha una relació senzilla entre els dos sistems lineals; en altres paraules, podem "post-processar els coeficients de sortida perquè corresponguin a la matriu inicial

# Nota: hi ha una rutina que calcula directament la pseudo-inversa (ho fa via la SVD):

library(MASS)

ginv(X)



#############################################
# EXEMPLE 2
# Anem a analitzar dades de greix corporal
# mitjançant regressió normal i ridge (regularitzada)
#############################################


bodyfat.data <- read.table(file = "bodyfatdata.txt", header=FALSE, col.names = c('triceps', 'thigh', 'midarm', 'bodyfat'))

summary(bodyfat.data)

attach(bodyfat.data)

N <- nrow(bodyfat.data)

## let us start with standard linear regression
## this time we directly use the method lm(); 
## lm() is actually called by glm() for gaussian noise an is the workhorse for least squares

(model <- lm(bodyfat ~ ., data = bodyfat.data))
summary(model)

## How to read this output:

# x = (1,triceps, thigh, midarm)^T
# w = (117.085, 4.334, -2.857, -2.186)^T

# the model is y(x; w) = w^T x = 117.085 + 4.334*triceps -2.857*thigh -2.186*midarm

# The residuals are the differences (t_n - y(x_n; w)), n = 1,..N
# let's inspect model$residuals

dens <- density(model$residuals)
hist(model$residuals, prob=T)
lines(dens,col="red")

# Do the residuals look Gaussian? this is direct indication of model validity
# (since it was our departing assumption)

# Let's do a more informative plot (a QQ-plot), which plots actual quantiles
# against theoretical quantiles of a comparison distribution (Gaussian in this case)

library(car)
qqPlot(model)

# The solid line corresponds to the theoretical quantiles
# therefore in this case the residuals are not even close 
# (the tails are heavier, the central part is flatter)

# This is how we can compute the mean square error
prediction <- predict(model)
(mean.square.error <- sum((bodyfat - prediction)^2)/N)

# Is this number large or small? it depends on the magnitude of the targets!
# a very good practice is to normalise it, by dividing by the variance of the target:

(norm.mse <- sum((bodyfat - prediction)^2) / ((N-1)*var(bodyfat)))

# If we divide the mean square error by the variance of the targets t,
# we get the proportion of the variability of the target that is NOT explained by the model

# A model with 'norm.mse' equal to 1 is as good as the best constant model
# (namely, the model that always outputs the average of the target)
# models with 'norm.mse' above 0.5 are so so, beyond 0.7 they begin to be quite bad
# models with 'norm.mse' below 0.2 are quite good

# The Multiple R-squared (usually used by statisticians) is obtained by subtracting this quantity form one; that is, the proportion of the target variability that is explained by the model; in this case it reaches 80%

(R.squared <- (1 - norm.mse)*100)

# The "adjusted R-squared" is the same thing, but adjusted for the complexity of the model,
# i.e. the number of parameters (three in our case)

# Now let us try to see how are the real predictions by plotting the real predictions against the targets:

plot(bodyfat, predict(model))

# It is difficult to see if the model is a good predictor; what we need is a
# numerical assessment of predictive ability. We compute the exact LOOCV as seen in class:

(LOOCV <- sum( (model$residuals/(1-ls.diag(model)$hat))^2) / N)

# and the corresponding predictive R-square 
(R2.LOOCV = (1 - LOOCV*N/((N-1)*var(bodyfat)))*100)

# we can see that prediction quality is not as good as it seemed (since CV error is worse than training error)

## this last number is the one I recommend to do model selection!!!

## Let us continue now with *regularized linear regression* (aka ridge regression)
## this time we need to use the method lm.ridge()

# We must first choose a value for lambda (the regularization constant)
# there are several criteria to do this, the most used of which is the GCV
# so we optimize the GCV for several values of lambda in a sequence

library(MASS)

# notice we start with a wide logarithmic search
lambdas <- 10^seq(-6,2,0.1)

select(lm.ridge(bodyfat ~ triceps + thigh + midarm, lambda = lambdas))

# best value (according to GCV) is 0.01995262

# we perform a finer search
lambdas <- seq(0,1,0.001)

select(lm.ridge(bodyfat ~ triceps + thigh + midarm, lambda = lambdas))

# Definitely the best value is 0.019, so we refit the model with this precise value:

(bodyfat.ridge.reg <- lm.ridge(bodyfat ~ triceps + thigh + midarm, lambda = 0.019))

# Now let us compare these results with those obtained by standard regression (without regularization)

# Hand calculation of coefficients, since we know the theory:

X <- cbind(rep(1,length=length(bodyfat)),triceps, thigh, midarm)

(w <- ginv(X) %*% bodyfat)

# call to lm(), check they should coincide

model$coefficients

# Notice that the regularized weights are smaller (in absolute value), one by one

# Now we calculate the corresponding prediction errors
# First by standard regression (without regularization, we already did this)

R2.LOOCV

# Now those with ridge regression (with regularization):

(1 - bodyfat.ridge.reg$GCV)*100

# The prediction errors are quite close and the model is way simpler: 
# we would probably prefer the regularized one
# 