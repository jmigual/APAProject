#####################################
# APA Laboratori 4                 ##
## LDA/QDA/NBayes/RegLog           ##
## version of November, 2016       ## 
#####################################
 

library(MASS) 


####################################################################
## Example 1: Visualizing and classifying wines with LDA and QDA
####################################################################

## We have the results of an analysis on wines grown in a region in Italy but derived from three different cultivars.
## The analysis determined the quantities of 13 chemical constituents found in each of the three types of wines. 
## The goal is to separate the three types of wines:

wine <- read.table("wine.data", sep=",", dec=".", header=FALSE)

dim(wine)

colnames(wine) <- c('Wine.type','Alcohol','Malic.acid','Ash','Alcalinity.of.ash','Magnesium','Total.phenols',
                    'Flavanoids','Nonflavanoid.phenols','Proanthocyanins','Color.intensity','Hue','OD280/OD315','Proline')

# Clean up column names
colnames(wine) <- make.names(colnames(wine))

wine$Wine.type <- as.factor(wine$Wine.type)

summary(wine)

plot(subset(wine,select=-Wine.type),col=unclass(wine$Wine.type))

## For this example let's practice a different call mode to lda(), using a formula; this is most useful
## when our data is in a dataframe format: 

lda.model <- lda (Wine.type ~ ., data = wine)

lda.model

## We can see that neither Magnesium or Proline seem useful to separate the wines; while
## Flavanoids and Nonflavanoid.phenols do. Ash is mainly used in the LD2.

## Plot the projected data in the first two LDs
## We can see that the discrimination is very good

plot(lda.model)

# alternatively, we can do it ourselves, with more control on color and text (wine number)

wine.pred <- predict(lda.model)
plot(wine.pred$x,type="n")
text(wine.pred$x,labels=as.character(rownames(wine.pred$x)),col=as.integer(wine$Wine.type))
legend('bottomright', c("Cultivar 1","Cultivar 2","Cultivar 3"), lty=1, col=c('black', 'red', 'green'), bty='n', cex=.75)

# If need be, we can add the (projected) means to the plot

plot.mean <- function (class)
{
  m1 <- mean(subset(wine.pred$x[,1],wine$Wine.type==class))
  m2 <- mean(subset(wine.pred$x[,2],wine$Wine.type==class))
  print(c(m1,m2))
  points(m1,m2,pch=16,cex=2,col=as.integer(class))
}

plot.mean ('1')
plot.mean ('2')
plot.mean ('3')

# indeed classification is perfect

table(wine$Wine.type, wine.pred$class)

# Let us switch to leave-one-out cross-validation

wine.predcv <- update(lda.model,CV=TRUE)
head(wine.predcv$posterior)
print(table(wine$Wine.type,wine.predcv$class))

# 2 mistakes (on 178 observations): 1.12% error

## Quadratic Discriminant Analysis is the same, replacing 'lda' by 'qda'
## problems may arise if for some class there are less (or equal) observations than dimensions
## (is not the case for the wine data)

qda.model <- qda (Wine.type ~ ., data = wine)

qda.model

## There is no projection this time (because projection is a linear operator and the QDA boundaries are quadratic ones)

# but let's have a look at classification:

wine.pred <- predict(qda.model)
table(wine$Wine.type, wine.pred$class)

# Let us switch to leave-one-out cross-validation

wine.predcv <- update(qda.model,CV=TRUE)
head(wine.predcv$posterior)

print(table(wine$Wine.type,wine.predcv$class))

# 1 mistake (on 178 observations): 0.56% error

# it would be nice to ascertain which wine is the "stubborn" one: it is a wine of type '2' classified
# as class '1'. Maybe there is something special with this wine ...

# In the event of numerical errors (insufficient number of observations per class), we can use 'rda'

library(klaR)
(rda.model <- rda (Wine.type ~ ., data = wine))

# Note gamma=0, lambda=1 corresponds to LDA

####################################################################
# Example 2: The Naïve Bayes classifier
####################################################################

library (e1071)

## Naive Bayes Classifier for Discrete Predictors: we use the 
## 1984 United States Congressional Voting Records; 

## This data set includes votes for each of the U.S. House of Representatives Congressmen on 16 key votes
## In origin they were nine different types of votes: 
##     * voted for, paired for, and announced for (these three simplified to yea or 'y'),
##     * voted against, paired against, and announced against (these three simplified to nay or 'n'), 
##     * voted present, voted present to avoid conflict of interest, and did not vote or otherwise make a position known 
##       (these three simplified to an 'unknown' disposition)

## The goal is to classify Congressmen as Republican or Democrat as a function of their voting profiles,
## which is not immediate because in the US Congressmen have a large freedom of vote 
## (obviously linked to their party but also to their own feelings, interests and compromises with voters)

data (HouseVotes84, package="mlbench") 

## add meaningful names to the votes
colnames(HouseVotes84) <- c("Class","handicapped.infants","water.project.sharing","budget.resolution",
                            "physician.fee.freeze","el.salvador.aid","religious.groups.in.schools","anti.satellite.ban",
                            "aid.to.nicaraguan.contras","mx.missile","immigration","synfuels.cutback",
                            "education.spending","superfund","crime","duty.free.exports","export.South.Sfrica")

summary(HouseVotes84)

## 1 = democrat, 0 = republican
## Note "unknown dispositions" have been treated as missing values!

set.seed(1111)

N <- nrow(HouseVotes84)

## We first split the available data into learning and test sets, selecting randomly 2/3 and 1/3 of the data
## We do this for a honest estimation of prediction performance

learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

# First we build a model using the learn data

model <- naiveBayes(Class ~ ., data = HouseVotes84[learn,])

# we get all the probabilities
model

# predict the outcome of the first 20 Congressmen
predict(model, HouseVotes84[1:20,-1]) 

# same but displaying posterior probabilities
predict(model, HouseVotes84[1:20,-1], type = "raw") 

# compute now the apparent error
pred <- predict(model, HouseVotes84[learn,-1])

# form and display confusion matrix & overall error
tab <- table(pred, HouseVotes84[learn,]$Class) 
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

# compute the test (prediction) error
pred <- predict(model, newdata=HouseVotes84[-learn,-1])

# form and display confusion matrix & overall error
tab <- table(pred, HouseVotes84[-learn,]$Class) 
tab
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

# note how most errors (9/12) correspond to democrats wrongly predicted as republicans

## in the event of empty empirical probabilities, this is how we would setup Laplace correction (aka smoothing): 
model <- naiveBayes(Class ~ ., data = HouseVotes84[learn,], laplace = 1)


####################################################################
# Example 3: The kNN classifier
####################################################################

## We are going to use the famous (Fisher's or Anderson's) Iris data set, which gives the measurements in centimeters
## of the sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of Iris. 
## The species are Iris setosa, versicolor, and virginica.

library (class)
data(iris3)

## first we split a separate test set of relative size 30%
learn.inputs   <- rbind(iris3[1:35,,1], iris3[1:35,,2], iris3[1:35,,3])
learn.classes <- factor(c(rep("s",35), rep("c",35), rep("v",35)))

test.inputs  <- rbind(iris3[36:50,,1], iris3[36:50,,2], iris3[36:50,,3])
test.classes <- factor(c(rep("s",15), rep("c",15), rep("v",15)))

## setup a kNN model with 3 neighbours
## Notice there is no "learning" ... the data is the model (just test!)

myknn <- knn (learn.inputs, test.inputs, learn.classes, k = 3, prob=TRUE) 

tab
tab <- table(myknn, test.classes) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

## rows are predictions, columns are true test targets

## one can use the function 'knn1()' when k=1 (just one neighbour)

## How do we optimize k? One way is by using LOOCV

myknn.cv <- knn.cv (learn.inputs, learn.classes, k = 3)

tab <- table(myknn.cv, learn.classes) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

## aha! now you see that previous training error (0%) was a little bit optimistic

## Let's loop over k
set.seed (23)

neighbours <- c(1:sqrt(nrow(learn.inputs)))
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (learn.inputs, learn.classes, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, learn.classes)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

errors

## It seems that k=8 is the best value
## Now "refit" with k=8 and predict the test set

myknn <- knn (learn.inputs, test.inputs, learn.classes, k = 8, prob=TRUE) 

tab
tab <- table(myknn, test.classes) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)

## so our error is 2.2%


####################################################################
# Example 4: Logistic Regression using artificial data
####################################################################


## The goal of this example is to get acquainted with the call to glm()
## glm() is used to fit generalized linear models (of which both linear and logistic regression are particular cases)

# You may need to recall at this point the logistic regression model ...

# Let x represent a single continuous predictor
# Let y represent a class ('0' or '1'), with a probability of being 1 that is related linearly to the predictor
# via the logit funtion, that is logit(p) = a*x + b (or beta_1*x + beta_0 if you prefer)

set.seed (1968)

N <- 500
x <- rnorm(n=N, mean=3, sd=2)     # generate the x_n (note x is a vector)
a <- 0.6 ; b <- -1.5              # this is the ground truth, which is unknown

p <- 1/(1+exp( -(a*x + b) ))      # generate the p_n (note p is a vector)
t <- rbinom(n=N,size=1,prob=p)
t <- as.factor(t)                 # generate the targets according to p

plot(x,t)

glm.res <- glm (t~x, family = binomial)

## look at the coefficients!
## 'Intercept' is b, 'x' is a
summary(glm.res)

## Obviously x is very significant (and the Intercept is always significant)

## Therefore, our estimated model is
## logit(p_n) = 0.60011*x_n -1.44926
## quite close to the ground truth

## In general you get this as:
##   glm.res$coefficients["x"] ; glm.res$coefficients["(Intercept)"]

## Interpretation of the coefficients:
## For a 1 unit increase in x, there is an increase in the odds for t by a factor of ...

exp(glm.res$coefficients["x"])

## that is almost doubling the odds (82.2% more)

####################################################################
## Example 5: Logistic regression for classifying spam mail
####################################################################

## This example will also illustrate how to change the 'cut point' for prediction, when there is an 
# interest in minimizing a particular source of errors

library(kernlab)  

data(spam)

## We do some basic pre-processing

spam[,55:57] <- as.matrix(log10(spam[,55:57]+1))

spam2 <- spam[spam$george==0,]
spam2 <- spam2[spam2$num650==0,]
spam2 <- spam2[spam2$hp==0,]
spam2 <- spam2[spam2$hpl==0,]

george.vars <- 25:28
spam2 <- spam2[,-george.vars]

moneys.vars <- c(16,17,20,24)
spam3 <- data.frame( spam2[,-moneys.vars], spam2[,16]+spam2[,17]+spam2[,20]+spam2[,24])

colnames(spam3)[51] <- "about.money"

dim(spam3)

set.seed (4321)
N <- nrow(spam3)                                                                                              
learn <- sample(1:N, round(0.67*N))
nlearn <- length(learn)
ntest <- N - nlearn

## Fit a GLM in the learning data
spamM1 <- glm (type ~ ., data=spam3[learn,], family=binomial)

## Simplify it using the AIC (this may take a while, since there are many variables)
spamM1.AIC <- step (spamM1)

# do not worry about these warnings: they are fitted probabilities numerically very close to 0 or 1

## We define now a convenience function:

# 'P' is a parameter; whenever our filter assigns spam with probability at least P then we predict spam
spam.accs <- function (P=0.5)
{
  ## Compute accuracy in learning data
  
  spamM1.AICpred <- NULL
  spamM1.AICpred[spamM1.AIC$fitted.values<P] <- 0
  spamM1.AICpred[spamM1.AIC$fitted.values>=P] <- 1
  
  spamM1.AICpred <- factor(spamM1.AICpred, labels=c("nonspam","spam"))
  
  print(M1.TRtable <- table(Truth=spam3[learn,]$type,Pred=spamM1.AICpred))
  
  print(100*(1-sum(diag(M1.TRtable))/nlearn))
  
  ## Compute accuracy in test data
  
  gl1t <- predict(spamM1.AIC, newdata=spam3[-learn,],type="response")
  gl1predt <- NULL
  gl1predt[gl1t<P] <- 0
  gl1predt[gl1t>=P] <- 1
  
  gl1predt <- factor(gl1predt, labels=c("nonspam","spam"))
  
  print(M1.TEtable <- table(Truth=spam3[-learn,]$type,Pred=gl1predt))
  
  print(100*(1-sum(diag(M1.TEtable))/ntest))
}

spam.accs()
# gives 7.21% TRAINING ERROR and 7.07% TESTING ERROR

## Although the errors are quite low still one could argue that we should try to lower the probability of predicting spam when it is not
# We can do this (at the expense of increasing the converse probability) by:

spam.accs(0.7)

# gives 9.66% TRAINING ERROR and 10.3% TESTING ERROR

## So we get a much better spam filter; notice that the filter has a very low probability of 
## predicting spam when it is not (which is the delicate case)
