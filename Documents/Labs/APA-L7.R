#####################################
# APA Laboratori 7                 ##
## Random Forests                  ##
## version of December, 2016       ## 
#####################################

set.seed (6046)

####################################################################
# Financial Example: classification model for deposit subscription
####################################################################

## Direct marketing campaigns (phone calls) of a Portuguese banking institution. 
## The classification goal is to predict if the client will subscribe a term deposit

deposit <- read.table ("bank-full.csv.gz", header=TRUE, stringsAsFactors=TRUE, sep=";")

dim(deposit)

summary(deposit)

## This dataset needs a lot of pre-processing ... also it displays a good mixture of categorical and numeric variables

# age seems OK
# job has 12 values, let's check their frequency

# seems OK
table(deposit$job)

# education has 4 values, let's check their frequency

# seems OK
table(deposit$education)

# month looks very suspicious ... but is OK

table(deposit$month)

# highly skewed ...

hist(deposit$duration)
hist(log(deposit$duration))

deposit$duration <- log(deposit$duration+0.001)

# what to do with 'pdays' and 'previous'? it is not clear how to best pre-process them; we shall need
# some financial expertise... we leave them as they are

## The rest seem OK (but it would take a careful analysis, and a lot of domain knowledge)

# I rename the target ...

colnames(deposit)[ncol(deposit)] <- "subscribed"

dim(deposit)

summary(deposit)

## Since we want to use different methods, we need CV and a separate test set:

library(TunePareto)

# precalculate the TR/TE partition and the cross-validation partitions on the TR part

N <- nrow(deposit)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- deposit[learn.indexes,]

nlearn <- length(learn.indexes)
ntest <- N - nlearn

## First try a standard decision tree (CART)

library(tree)

model.tree <- tree (subscribed ~ ., data = learn.data)

summary(model.tree)

# so training error rate is 10.72%

model.tree

plot (model.tree)
text (model.tree,pretty=0)

pred.tree <- predict (model.tree, deposit[test.indexes,], type="class")

(ct <- table(Truth=deposit$subscribed[test.indexes], Pred=pred.tree))

## We define now a convenience function (the harmonic mean), to compute the F1 accuracy:

harm <- function (a,b) { 2/(1/a+1/b) }

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

# not very good, because the 'yes' class is nearly ignored
(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## Now a random Forest
library(randomForest)

model.rf1 <- randomForest(subscribed ~ ., data = learn.data, ntree=100, proximity=FALSE)

model.rf1

## We get an estimated test error (OOB) of 9.3%, so better; let's compute the real test error:

pred.rf1 <- predict (model.rf1, deposit[test.indexes,], type="class")

(ct <- table(Truth=deposit$subscribed[test.indexes], Pred=pred.rf1))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## So OOB really works in estimating prediction error and the RF is better than a single tree; 
## however, there is a big issue in unbalanced classes

# one way to deal with this is to include class weights
model.rf2 <- randomForest(subscribed ~ ., data = learn.data, ntree=100, proximity=FALSE, classwt=c(1,10))

model.rf2

# which helps a little bit, but not much: we get estimated test error (OOB) of 9.86% with a better balance; let's compute the real test error:

pred.rf2 <- predict (model.rf2, deposit[test.indexes,], type="class")

(ct <- table(Truth=deposit$subscribed[test.indexes], Pred=pred.rf2))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

# another way is to stratify the sampling in the boostrap resamples

# 'yes' is the less represented class, so we upsample it

n.yes <- table(learn.data$subscribed)["yes"]
n.no <- table(learn.data$subscribed)["no"]

model.rf3 <- randomForest(subscribed ~ ., data = learn.data, ntree=100, proximity=FALSE, 
                          sampsize=c(yes=3000, no=3000), strata=learn.data$subscribed)

model.rf3

# which seems to help much more: we get estimated test error (OOB) of 14.4% with a very good balance
# let's compute the real test error:

pred.rf3 <- predict (model.rf3, deposit[test.indexes,], type="class")

(ct <- table(Truth=deposit$subscribed[test.indexes], Pred=pred.rf3))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## Now we can try to optimize the number of trees, guided by OOB:

(ntrees <- round(10^seq(1,3,by=0.2)))

# prepare the structure to store the partial results

rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in ntrees)
{ 
  print(nt)

  model.rf <- randomForest(subscribed ~ ., data = learn.data, ntree=nt, proximity=FALSE, 
                          sampsize=c(yes=3000, no=3000), strata=learn.data$subscribed)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]

  ii <- ii+1
}

rf.results

# choose best value of 'ntrees'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])

# we could also try to optimize the number of variables in the same way, though the default values work quite well in general

## Now refit the RF with the best value of 'ntrees'

model.rf <- randomForest(subscribed ~ ., data = learn.data, ntree=ntrees.best, proximity=FALSE, 
                         sampsize=c(yes=3000, no=3000), strata=learn.data$subscribed)

## let's compute the real test error:

pred.rf.final <- predict (model.rf, deposit[test.indexes,], type="class")

(ct <- table(Truth=deposit$subscribed[test.indexes], Pred=pred.rf.final))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

print(model.rf)

# The importance of variables
importance(model.rf)
varImpPlot(model.rf)

## 'duration' is the most important variable, then month, etc

# plot error rate: black = out of bag (OOB), red = label 1 ('no'), green  = label 2 ('yes')
# as a function of the number of trees used
plot(model.rf)

legend("topright", legend=c("OOB", "no", "yes"),    
       pch=c(1,1), col=c("black","red","green"))

# What variables are being used in the forest (their counts)
varUsed(model.rf, by.tree=FALSE,count = TRUE)

