#####################################
## APA Laboratori 2                ##
## Visualitzacio i Clustering      ##
## version of October, 2016        ## 
#####################################

set.seed(222)
library(MASS)

###########################################################
## Example 1: Comparison between PCA and LDA on 2D toy data
###########################################################

## Fisher's discriminant analysis (FDA) is a method that finds a linear combination of features to project or separate two or more classes of objects

## If your goal is to perform (linear) dimensionality reduction for class discrimination, you should use FDA instead of PCA; PCA is useful for signal representation (but not necessarily for discrimination)

## Sigma is a 2x2 positive-definite symmetric matrix specifying the covariance matrix of two variables

N <- 200

(Sigma <- matrix(data=c(2,1.3,1.3,1),nrow=2,ncol=2,byrow=TRUE))

# these are the eigenvalues:

eigen (Sigma, only.values=TRUE)$values

# Let's create class 1 ('red' class)

mean.1 <- matrix(c(2,0),nrow=2,ncol=1)                         

X.red <- mvrnorm(N,mu=mean.1,Sigma=Sigma)

# Let's create class 2 ('green' class)

mean.2 <- -mean.1

X.green <- mvrnorm(N,mu=mean.2,Sigma=Sigma)

par(mfrow=c(2,2))

plot(c(X.red[,1],X.green[,1]), c(X.red[,2],X.green[,2]), 
     col=c(rep('red',N),rep('green',N)), main="Toy data", xlab="X1", ylab="X2")

# Now we glue both classes one after the other and create a dataframe

d <- data.frame(c(rep(1,N),rep(2,N)), c(X.red[,1], X.green[,1]), c(X.red[,2], X.green[,2]))
colnames(d) <- c("target", "X1", "X2")
d$target <- as.factor(d$target)
summary(d)

# call to FDA (also known as LDA, because it is linear)
myLDA <- lda(d[c(2,3)],d[,1])

# Now we show the best projection direction on the original space. This direction maximizes the separability of the classes. For that, we first need the slope:

LDAslope <- myLDA$scaling[2]/myLDA$scaling[1]

# And now we can perform the visualization:

plot(c(X.red[,1],X.green[,1]), c(X.red[,2],X.green[,2]), col=c(rep('red',N),rep('green',N)),
     main="Direction for projection using FDA", xlab="X1", ylab="X2")

abline(0,LDAslope,col='black',lwd=2)

# We can also compute the projections of the two classes

myLDA.proj <- d[,2] * myLDA$scaling[1] + d[,3] * myLDA$scaling[2]

plot(myLDA.proj, c(rep(0,N),rep(0,N)), col=c(rep('green',N),rep('red',N)),
     main='FDA projection as seen in 1D', xlab="Discriminant", ylab="")

# To understand what is going on, do:

myLDA

# of which ...

myLDA$scaling

# ... are the coefficients of the linear discriminant

## So we are projecting the data into the direction that maximizes (linear) separability:

# projection(X) = X1*myLDA$scaling[1] + X2*myLDA$scaling[2]

# Now we compute PCA:
  
myPCA <- prcomp(d[c(2,3)],scale=TRUE)

# Now we need to project the data in the first principal component

d1PCA <- myPCA$x[,1]
PCAslope1 <- myPCA$rotation[2,1]/myPCA$rotation[1,1]


# And now we can perform the visualization:

plot(c(X.red[,1],X.green[,1]), c(X.red[,2],X.green[,2]), col=c(rep('red',N),rep('green',N)),
     main="Direction for projection using PCA", xlab="X1", ylab="X2")

abline(0,PCAslope1,col='black',lwd=2)

# We can see that the FDA projection maximices separability while the PCA projection maximices variability

par(mfrow=c(1,1))

####################################################################
## Example 2: Visualizing crabs with FDA
####################################################################

# Campbell studied rock crabs of the genus "Leptograpsus" in 1974. One
# species, Leptograpsus variegatus, had been split into two new species,
# previously grouped by colour (orange and blue). Preserved specimens
# lose their colour, so it was hoped that morphological differences
# would enable museum material to be classified.

# Data is available on 50 specimens of each sex of each species (so 200 in total),
# collected on sight at Fremantle, Western Australia. Each specimen has
# measurements on: the width of the frontal lobe (FL), the rear width (RW),
# the length along the carapace midline (CL), the maximum width (CW) of 
# the carapace, and the body depth (BD) in mm, in addition to
# colour (that is, species) and sex.

## the crabs data is also in the MASS package
data(crabs)

## look at data
?crabs
summary(crabs)
head(crabs)

## The goal is to separate the 200 crabs into four classes, given by the 2x2 configurations for sex (Male/Female) and species (Blue/Orange)

Crabs.class <- factor(paste(crabs[,1],crabs[,2],sep=""))

# Now 'BF' stands now for 'Blue Female', and so on
table(Crabs.class)

## using the rest of the variables as predictors (except 'index', which is only an index)
Crabs <- crabs[,4:8]

summary(Crabs)

## Various preliminary plots (notice all 5 predictors are continuous)

par(mfrow=c(1,1))
boxplot(Crabs)

hist(Crabs$FL,col='red',breaks=20,xlab="", main='Frontal Lobe Size (mm)')
hist(Crabs$RW,col='red',breaks=20,xlab="", main='Rear Width (mm)')
hist(Crabs$CL,col='red',breaks=20,xlab="", main='Carapace Length (mm)')
hist(Crabs$CW,col='red',breaks=20,xlab="", main='Carapace Width (mm)')
hist(Crabs$BD,col='red',breaks=20,xlab="", main='Body Depth (mm)')

## Now let's visualize data using FDA

(lda.model <- lda (x=Crabs, grouping=Crabs.class))

plot(lda.model)

## As there are four classes (called 'groups' in LDA), we get three linear discriminants (LDs) for projection (always the number of classes minus 1)
## We first compute the loadings (the 'loadings' are simply the projected data)

## This time we do it more generally, using matrix multiplication

loadings <- as.matrix(Crabs) %*% as.matrix(lda.model$scaling)

## We are performing dimensionality reduction 5D --> 3D, and plotting the projected data into the first two LDs (the 2 most important dimensions)

# We do our own plotting method, with color and legend:

colors.crabs <- c('blue', 'lightblue', 'orange', 'yellow')

crabs.plot <- function (myloadings)
{
  plot (myloadings[,1], myloadings[,2], type="n", xlab="LD1", ylab="LD2")
  text (myloadings[,1], myloadings[,2], labels=crabs$index, col=colors.crabs[unclass(Crabs.class)], cex=.55)
  legend('topright', c("B-M","B-F","O-M","O-F"), fill=colors.crabs, cex=.55)
}

crabs.plot (loadings)

# The result is quite satisfactory, right? We can see that the 5 continuous predictors do indeed represent 4 different crabs. 

# We can also see that crabs of the Blue "variety" are less different 
# (regarding males and females) than those in the Orange variety

## If you would like to keep this new representation for later use (maybe to build a classifier on it), simply do:

Crabs.new <- data.frame (New.feature = loadings, Target = Crabs.class)

summary(Crabs.new)

## Now let's analyze the numerical output of lda() in more detail:

lda.model

# "Prior probabilities of groups" is self-explanatory (these are estimated from the data, but can be overriden by the 'prior' parameter)

# "Group means" is also self-explanatory (these are our mu's)

# "Coefficients of linear discriminants" are the scaling factors we have been using to project data. These have been normalized so that the within-groups covariance matrix is spherical (a multiple of the identity). 

# This means that the larger the coefficient of a predictor,
# the more important the predictor is for the discrimination:

lda.model$scaling

# We can interpret our plot so that the horizontal axis (LD1) separates the groups mainly by using FL, CW and BD;
# the vertical axis (LD2) separates the groups mainly by using RW and some CL, etc

## The "Proportion of trace" is the proportion of between-class variance that is explained by successive discriminants (LDs)

# For instance, in our case LD1 explains 68.6% of the total between-class variance

## In this case, the first two LDs account for 98.56% of total between-class variance, fairly close to 100%

## This means that the third dimension adds but a little bit of discriminatory information. Let's visualize the crabs in 3D:

library(rgl)

# 3D scatterplot (can be rotated and zoomed in/out with the mouse)
plot3d(loadings[,1], loadings[,2], loadings[,3], "LD1", "LD2", "LD3",
       size = 4, col=colors.crabs[unclass(Crabs.class)], main="Crabs Data")

text3d(loadings[,1], loadings[,2], loadings[,3], color = "black", texts=rownames(Crabs.new), adj = c(0.85, 0.85), cex=0.6)
         
## As the measurements are lengths, it could be sensible to take logarithms

(lda.logmodel <- lda (x=log(Crabs), grouping=Crabs.class))

## The model looks a bit better, given that he first two LDs now account for 99.09% of total between-class variance, very good indeed, so a 3D plot does not add anything visual

## As an example, the first (log) LD is given by:
# LD1 = -31.2*log(FL) - 9.5*log(RW) - 9.8*log(CL) + 66*log(CW) - 18*log(BD)

## get the new loadings

logloadings <- as.matrix(log(Crabs)) %*% as.matrix(lda.logmodel$scaling)

## plot the projected data in the first two LDs

crabs.plot (logloadings)

## The first coordinate clearly expresses the difference between species, and the second the difference between sexes!

####################################################################
# Example 3. Clustering easy artificial 2D data with k-means
####################################################################

## First we create a simple data set:

# the cclust library contains some clustering functions, including k-means
library (cclust)

N1 <- 30
N2 <- 40
N3 <- 50

# create cluster 1
x1 <- rnorm (N1,1,0.5)
y1 <- rnorm (N1,1,0.5)

# create cluster 2
x2 <- rnorm (N2,2,0.5)
y2 <- rnorm (N2,6,0.7)

# create cluster 3
x3 <- rnorm (N3,7,1)
y3 <- rnorm (N3,7,1)

# create the data
x <- rbind (cbind(x1,y1), cbind(x2,y2), cbind(x3,y3))
c <- c(rep("4", N1), rep("2", N2), rep("3", N3))
D <- data.frame (x,color=c)

## this is your data
plot(D$x1,D$y1)

## and these are the true clusters
plot(D$x1,D$y1,col=as.vector(D$color))

# so we have 3 very clean clusters ...

# Let's execute k-means

K <- 3 # yeah, this is tricky, why 3?

## execute k-means with a maximum of 100 iterations
kmeans.3 <- cclust (x,K,iter.max=100,method="kmeans",dist="euclidean")

## plot initial and final prototypes (cluster centers)
points(kmeans.3$initcenters)
points(kmeans.3$centers)

## draw arrows to see the process
arrows (kmeans.3$initcenters[,1], kmeans.3$initcenters[,2], kmeans.3$centers[,1], kmeans.3$centers[,2])

## plot and paint the clusters (according to the computed assignments)
plot(D$x1,D$y1,col=(kmeans.3$cluster+1))

## plot the cluster centers
points(kmeans.3$centers,col=seq(1:kmeans.3$ncenters)+1,cex=2,pch=19)

# clustering quality as measured by the Calinski-Harabasz index (recommended)
# This index measures the dispersion of the data points within the clusters (SSW) and between the clusters (SSB)
# A good clustering has small SSW (compact clusters) and large SSB (separated cluster centers)
# There is also a correction for the number of clusters

# The C-H index is then:

# C-H = (SSB/(K-1)) / (SSW/(N-K))

# where N is the number of data points and K is the number of clusters

(CH.3 <- clustIndex(kmeans.3,x, index="calinski"))

## now let's not be tricky ##

K <- 5 # guess what is going to happen?

## execute k-means with a maximum of 100 iterations
kmeans.5 <- cclust (x,K,iter.max=100,method="kmeans",dist="euclidean")

## this is your data again
plot(D$x1,D$y1,col=as.vector(D$color))

## plot initial and final prototypes (centers)
points(kmeans.5$initcenters)
points(kmeans.5$centers)

## draw arrows to see the process
arrows (kmeans.5$initcenters[,1], kmeans.5$initcenters[,2], kmeans.5$centers[,1], kmeans.5$centers[,2])

## plot and paint the clusters (according to the computed assignments)
plot(D$x1,D$y1,col=(kmeans.5$cluster+1))

## plot the cluster centers
points(kmeans.5$centers,col=seq(1:kmeans.5$ncenters)+1,cex=2,pch=19)

# clustering quality as measured by the Calinski-Harabasz index

(CH.5 <- clustIndex(kmeans.5,x, index="calinski"))

# notice CH.3 > CH.5, so K=3 is better according to C-H

###########################################################################
# Example 4. Clustering not-so-easy artificial 2D data with k-means and E-M
###########################################################################

## the MASS library contains the multivariate gaussian
library(MASS)

## the ggplot2 library contains functions for making nice plots
library(ggplot2)

set.seed(333)

## First we need some auxiliary functions

# GENERATE DATA FROM A MIXTURE OF 2D GAUSSIANS
generate.data <- function(N, K, prior.mean, prior.var)
{
  p <- length(prior.mean)
  
  # generate random mixture centres from the prior
  mu_k <- mvrnorm(K, mu=prior.mean, Sigma=diag(prior.var, 2))
  
  # generate mixture coefficients
  pi_k <- runif(K)
  pi_k <- pi_k/sum(pi_k)
  
  # generate the data
  obs <- matrix(0, nrow=N, ncol=p)
  z <- numeric(N)
  sigma_k <- matrix(0, nrow=K, ncol=p)
  
  for (i in 1:K)
    sigma_k[i,] <- runif(p)
  
  for (i in 1:N)
  {
    # draw the observation from a component according to coefficient
    z[i] <- sample(1:K, 1, prob=pi_k)
    # draw the observation from the corresponding mixture location
    obs[i,] <- mvrnorm(1, mu=mu_k[z[i],], Sigma=diag(sigma_k[z[i],],p))
  }
  list(locs=mu_k, z=z, obs=obs, coefs=pi_k)
}

# plot 2d data from a mixture
plot.mixture <- function(locs, z, obs)
{
  stopifnot(dim(obs)[2]==2)
  z <- as.factor(z)
  df1 <- data.frame(x=obs[,1], y=obs[,2], z=z)
  df2 <- data.frame(x=locs[,1], y=locs[,2])
  p <- ggplot()
  p <- p + geom_point(data=df1, aes(x=x, y=y, colour=z), shape=16, size=2, alpha=0.75)
  p <- p + geom_point(data=df2, aes(x=x, y=y), shape=16, size=3)
  p <- p + theme(legend.position="none")
  p
}

# plot 2D data as a scatter plot
plot.data <- function(dat)
{
  stopifnot(dim(dat)[2]==2)
  df1 <- data.frame(x=dat[,1], y=dat[,2])
  p <- ggplot()
  p <- p + geom_point(data=df1, aes(x=x, y=y), size=2, alpha=0.75)
  p
}

## Let us generate the data

N <- 1000
K <- 5
centre <- c(0,0)
dispersion <- 10

## generate 2D data as a mixture of 5 Gaussians, each axis-aligned (therefore the two variables are independent)
## with different variances
## the centers and coefficients of the mixture are chosen randomly
d <- generate.data (N,K,centre,dispersion)

## these are the components of the mixture
plot.mixture(d$locs, d$z, d$obs)

## may be we want to have a look at the unconditional density p(x)

## compute 2D kernel density
z <- kde2d(d$obs[,1], d$obs[,2], n=50)

## some pretty colors
library(RColorBrewer)
colorets <- rev(brewer.pal(11, "RdYlBu"))

## this is the raw data (what the clustering method sees)
plot(d$obs, xlab="x", ylab="y", pch=19, cex=.4)

## and this is a contour plot of the unconditional density 
contour(z, drawlabels=FALSE, nlevels=22, col=colorets, add=TRUE)
abline(h=mean(d$obs[,2]), v=mean(d$obs[,1]), lwd=2)

## a simpler way of plotting the data
plot.data(d$obs)


######################################
## let us try first with k-means (K=2)

K <- 2

kmeans2.2 <- cclust (d$obs,K,iter.max=100,method="kmeans",dist="euclidean")

plot(d$obs[,1],d$obs[,2],col=(kmeans2.2$cluster+1))
points(kmeans2.2$centers,col=seq(1:kmeans2.2$ncenters)+1,cex=2,pch=19)

## I recommend you maximize the plot window
## Can we be indulgent with the result? we know the truth is there are 5 clusters,
## Is this is a reasonable result if we ask for 2?

# clustering quality as measured by the Calinski-Harabasz index

(CH2.2 <- clustIndex(kmeans2.2,d$obs, index="calinski"))

## let us try now with k-means (K=5)

K <- 5

kmeans2.5 <- cclust (d$obs,K,iter.max=100,method="kmeans",dist="euclidean")

plot(d$obs[,1],d$obs[,2],col=(kmeans2.5$cluster+1))
points(kmeans2.5$centers,col=seq(1:kmeans2.5$ncenters)+1,cex=2,pch=19)

## I recommend you maximize the plot window
## This time the result has even more chances of being largely incorrect
## because there are more ways of getting a wrong solution

# clustering quality as measured by the Calinski-Harabasz index

(CH2.5 <- clustIndex(kmeans2.5,d$obs, index="calinski"))

# at least CH2.5 >> CH2.2 ... so C-H does a good job

## In class we saw that k-means is usually re-run several times

do.kmeans <- function (whatK)
{
  r <- cclust (d$obs,whatK,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,d$obs, index="calinski"))
}

max (r <- replicate (100, do.kmeans(5)))

# so it is not a matter of wrong initialization
# this is really the best k-means can do here

# this may take a while
res <- vector("numeric",10)
for (K in 2:10)
  res[K] <- max (r <- replicate (100, do.kmeans(K)))

plot(res, type="l")

## the conclusion is that k-means + C-H bet for 5 clusters ... 
## not bad, not bad ...

## but the real *shape* of the clusters cannot be captured, because k-means only "sees" spherical clusters and these are ellipsoidal

######################################
## let us try now E-M

library(Rmixmod)

## This method performs E-M for mixture densities, including mixtures of Gaussians
## we can specify which family of gaussians we intend to fit:

## "general" for the general family, "diagonal" for the diagonal family, 
## "spherical" for the spherical family and "all" for all families (meaning the union)
## WARNING: default is "general".

## suppose first that we know the truth and specify axis-aligned densities (i.e., independent variables)

fammodel <- mixmodGaussianModel (family="diagonal", equal.proportions=FALSE)

z <- mixmodCluster (data.frame(d$obs), models = fammodel, nbCluster = 5)

summary(z)

# the final centers
means <- z@bestResult@parameters@mean

# if you want hard assignments
found.clusters <- z@bestResult@partition

# other interesting outcomes are:

## the estimated covariance matrices for each cluster
z@bestResult@parameters@variance

## self-explained
z@bestResult@likelihood 

## the posterior probabilities = soft assignments = the gamma_k(x_n) in class
z@bestResult@proba

## This is a graphical summary of the clustering

plot(d$obs[,1],d$obs[,2],col=(found.clusters+1))
points(means,col=seq(1:5)+1,cex=2,pch=19)

## it was very likely that E-M performed extremely well
## why? because we knew the truth (cluster form and number)

## suppose now we do not the truth but we still wish to fit general gaussians

fammodel <- mixmodGaussianModel (family="general", equal.proportions=FALSE)

z <- mixmodCluster (data.frame(d$obs),models = fammodel, nbCluster = 5)

summary(z)

means <- z@bestResult@parameters@mean
found.clusters <- z@bestResult@partition

plot(d$obs[,1],d$obs[,2],col=(found.clusters+1))
points(means,col=seq(1:5)+1,cex=2,pch=19)

## the method works also very smoothly
## why? because we data *is* gaussian

## compare the estimated centers
means

# with the truth (note the clusters may appear in a different order)
d$locs

## or the estimated coefficients
sort(z@bestResult@parameters@proportions)

# with the truth 
sort(d$coefs)
