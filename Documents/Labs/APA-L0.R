####################################################################
# APA Laboratori 0 (Grau FIB)
# Lluís A. Belanche

# Data preprocessing
# version of September 2016
####################################################################


################################### INSTRUCCIONS
# No cal entregar res, cal assimilar

# S'ha de fer amb calma, sense córrer, mirant d'entendre en tot moment què s'està fent --a nivell conceptual-- i quin efecte té

# Podeu deixar el codi R en si (crides, paràmetres, sintaxi) per una segona lectura o com a treball personal. El codi R és un mitjà, no un objectiu.
###################################


####################################################################
# SECTION 1: READING THE FILE CREDSCO.TXT (loan data: credit scoring)
####################################################################

# Reading properly a data set is non-trivial because you need to know
# its data format: decimal separator, column separator, is there a
# header? how are strings quoted? how (if any) are missing values
# coded? should character vectors be converted to factors? should
# white spaces be stripped?, ...)

# It is a good idea to consult ?read.csv and play with useful control parameters, like

# header=TRUE
# na.strings="?"
# dec = "."
# sep = ";"
# quote = "\"

# .. and some others

# after opening the file "credsco.csv" and inspecting it, we decide the following settings:

Credit <- read.csv("credsco.csv", header = TRUE, quote = "\"", dec = ".", check.names=TRUE)

# the dimensions of the data set are 

dim(Credit)

# which means 4,455 examples described by 14 variables

# Basic questions:

# Which is the target variable? where is it? how many different values? is it a classification problem or a regression problem?

# answers: the target variable is located in column 1 and is called 'Assessment'; it has two possible values (therfore it is a classification problem)

# What are the other variables?

names(Credit)

# You can consult the file "Credsco-traduccions.txt" for translation into Catalan

# inspect the first 4 examples

Credit[1:4,]

# inspect predictive variables 4, 5, 6 and 7 for the first example

Credit[1,5:8]

####################################################################
# SECTION 2: BASIC INSPECTION OF THE DATASET
####################################################################

# Perform a basic inspection of the dataset. Have a look at the minimum and maximum values for each variable; find possible errors and abnormal values (outliers); find possible missing values; decide which variables are continuous and which are categorical; if there are mixed types, we have three options: recode continuous to categorical, recode categorical to continuous or leave them as they are. In the latter case, either the method accepts both kinds of information, or it does not, in which case R will convert the categorical ones to continuous using a dummy code.

summary(Credit)

# Assessment,Housing,MaritalStatus,Records,TypeOfJob are categorical and need to be treated properly
# In particular, Assessment is the target variable; we need to identify correct values

# Capital, ChargesOnCapital and Income present abnormally high maximums (99999999)
# There are also suspicious zeros, in both types of variables, which we identify with missing values

####################################################################
# SECTION 3: DEALING WITH MISSING VALUES
####################################################################

# Sometimes we need to take a decision on a sensible treatment for the missing values and apply it; it is wise to write down the possible consequences of this decision and the alternatives that could be considered in case the final results are not satisfactory

# the easiest way is of course to eliminate the involved rows or
# columns; this can be done partially. For example, we could decide to
# eliminate the variables with the highest proportion of missing values.
# Deleting instances and/or variables containing missing values results
# in loss of relevant data and is also frustrating because of the effort
# in collecting the sacrificed information.

# CAREFUL! R does not know magically which entries are missing values: they have to be explicitly declared as NA's

# therefore this code is not useful:

?na.omit

Credit.complete <- na.omit(Credit)
dim(Credit.complete)

# the previous code does nothing! (but it seems it does)

# In the present case we have decided to perform a step-by-step treatment, separate for the categorical and continuous information

# We first decide to remove those rows with with missing values in the categorical variables (there are few)

table(Credit[,1]==0)
table(Credit[,3]==0)
table(Credit[,6]==0)
table(Credit[,8]==0)

Credit <- Credit[Credit[,1] != 0 & Credit[,3] != 0 & Credit[,6] != 0 & Credit[,8] != 0,]

dim(Credit)

# Process rows with missing values in the continuous variables (code 99999999)

attach(Credit) # this allows the column names of Credit to be visible (use with care)

# look at that:

hist(Income)
hist(Income[Income != 99999999])
hist(Income[Income != 99999999 & Income != 0], breaks=15)

# these are then clearly incorrect

table(Income == 99999999)

table(Income == 0)

table(Capital == 99999999)

table(ChargesOnCapital == 99999999)

# what do we do with this one? let's assume it is correct

table(YearsInJob == 0)


# Continuous variables have too many missing values, we can not eliminate them just like that: we must devise a treatment for these missing values

# first we mark them to 'NA', including those from no 'Income'

Income[Income == 99999999 | Income == 0] <- NA
Capital[Capital == 99999999] <- NA
ChargesOnCapital[ChargesOnCapital == 99999999] <- NA

# see the difference

summary(Credit[,10])
summary(Income)

# The word 'imputation' refers to assigning a value to every missing value. Here we perform imputation by a method known as 1NN: for every individual with a missing 'Income', we look for the most similar individual (according to the remaining variables) and then copy its 'Income' value

library(class) # knn

# Imputation of 'Income'

aux <- Credit[,-10]
dim(aux)
aux1 <- aux[!is.na(Income),]
dim(aux1)
aux2 <- aux[is.na(Income),]
dim(aux2)

# Neither of aux1, aux2 can contain NAs
knn.inc <- knn(aux1,aux2,Income[!is.na(Income)])
Income[is.na(Income)] <- as.numeric(as.character(knn.inc))

# Imputation of 'Capital'
aux <- Credit[,-11]
aux1 <- aux[!is.na(Capital),]
aux2 <- aux[is.na(Capital),]
knn.cap <- knn(aux1,aux2,Capital[!is.na(Capital)])
Capital[is.na(Capital)] <- as.numeric(as.character(knn.cap))

# Imputation of 'ChargesOnCapital'
aux <- Credit[,-12]
aux1 <- aux[!is.na(ChargesOnCapital),]
aux2 <- aux[is.na(ChargesOnCapital),]
knn.cac <- knn(aux1,aux2, ChargesOnCapital[!is.na(ChargesOnCapital)])
ChargesOnCapital[is.na(ChargesOnCapital)] <- as.numeric(as.character(knn.cac))

ChargesOnCapital[Capital==0] <- 0

# assign back to the dataframe

Credit[,10] <- Income
Credit[,11] <- Capital
Credit[,12] <- ChargesOnCapital

# inspect again the result, especially the new statistics

dim(Credit)
summary(Credit)

####################################################################
# SECTION 4: TREATMENT OF MIXED DATA TYPES
####################################################################

# In this case we have decided to keep the original type and leave the decision for later, depending on the specific analysis

# we explicitly declare categorical variables as such (called 'factors' in R)

Assessment    <- as.factor(Assessment)
Housing     <- as.factor(Housing)
MaritalStatus <- as.factor(MaritalStatus)
Records   <- as.factor(Records)
TypeOfJob <- as.factor(TypeOfJob)

levels(Assessment)
levels(Housing)
levels(MaritalStatus)
levels(Records)
levels(TypeOfJob)

# not very nice, right? let's recode

levels(Assessment) <- c("positive","negative")
levels(Housing) <- c("rent","owner","private","ignore","parents","other")
levels(MaritalStatus) <- c("single","married","widower","split","divorced")
levels(Records) <- c("no","yes")
levels(TypeOfJob) <- c("indefinite","temporal","self-employed","other")

# WARNING! some R programmers do not like 'attach', look what happens

is.factor(Assessment)
is.factor(Credit[,1])

# (we'll fix this later)

####################################################################
# SECTION 5: DERIVATION OF NEW VARIABLES: FEATURE EXTRACTION
####################################################################

# We decide whether it can be sensible to derive new variables; we extract two new continuous and one new categorical variable (for the sake of illustration):

# Financing ratio (continuous)

FinancingRatio <- 100*AmountRequested/MarketPrice

hist(FinancingRatio)

# Saving capacity (continuous)

SavingCapacity <- (Income-Expenses-(ChargesOnCapital/100))/(AmountRequested/Deadline)

hist(SavingCapacity, breaks=16)

# Amount Requested greater than the median by people younger than 1.25 times the mean (categorical):

Dubious <- rep("No", nrow(Credit))
Dubious[AmountRequested > median(AmountRequested, na.rm = TRUE) & Age < 1.25*mean (Age, na.rm = TRUE)] <- "Yes"
Dubious <- as.factor(Dubious)

table(Dubious,Assessment)


####################################################################
# SECTION 6: WHAT WE HAVE DONE SO FAR
####################################################################

# Create a new dataframe that gathers everything and inspect it again

Credit.new <- data.frame(Assessment,YearsInJob,Housing,Deadline,Age,MaritalStatus,Records,TypeOfJob,Expenses,Income,Capital,ChargesOnCapital,AmountRequested,MarketPrice,FinancingRatio,SavingCapacity,Dubious)
                   
summary(Credit.new)
dim(Credit.new)

detach(Credit)   ## this undoes the attach (recommended)

attach(Credit.new)
is.factor(Credit.new[,1])

####################################################################
# SECTION 7: GAUSSIANITY AND TRANSFORMATIONS
####################################################################

# Perform a graphical summary of some of the variables (both categorical and continuous), using the boxplot() and hist() procedures

# For continuous data:
# histograms and boxplots

hist(Income)
hist(Income,col=2)

hist(Capital)
hist(log10(Capital), breaks=20)

boxplot (Deadline)
title ("These are the deadlines")

boxplot (Age, col = "lightgray")
title ("and these are the ages")

boxplot(Credit.new[,9:16], outline=TRUE) 
boxplot(Credit.new[,9:16], outline=FALSE) # much better, but would be nicer one by one

# the previous plots suggest to take logs on some variables: Capital and ChargesOnCapital (we'll do it later)

# For categorical data:
# Frequency tables, Contingency tables, Bar charts, Pie charts

# should we treat Age as categorical? probably not

table(Age)                            

min(Age)                                                          
max(Age)                                                          
Age.cat <- cut(Age, breaks = seq(30, 90, 10)) # WARNING! we are generating NAs               
Age.cat

Age.cat <- cut(Age, breaks = seq(15, 75, 10))   
Age.tab <- table(Age.cat)                              
Age.tab
barplot(Age.tab)                                     # bar chart
pie(Age.tab)                                         # pie chart

# incidentally, this is how we could generate another new variable based on Age:

Age2.cat <- factor(as.integer(Age < 55))        
levels(Age2.cat) <- c("over55","under55")

TypeOfJob.Age <- table(TypeOfJob, Age.cat)          # contingency table
TypeOfJob.Age
margin.table(TypeOfJob.Age, 1)                      # row sums
margin.table(TypeOfJob.Age, 2)                      # column sums

prop.table(TypeOfJob.Age)                           # relative freqencies
round(prop.table(TypeOfJob.Age), digits=3)          # idem, rounded to 3 digits
round(prop.table(TypeOfJob.Age) * 100, digits=3)    # total percentages

round(prop.table(TypeOfJob.Age, 1), digits=3)       # table of relative frequencies (row-wise)
TypeOfJob.Age.rel <- round(prop.table(TypeOfJob.Age, 2), digits=3)      # table of relative frequencies (column-wise)

barplot(TypeOfJob.Age)                                # basic stacked bar chart

barplot(TypeOfJob.Age.rel, yaxt="n", xlab="Age", ylab="proportion", 
        col = c("white", "grey80", "grey40", "black"), 
        main = "TypeOfJob by Age", xlim=c(0,9))          # stacked bar chart

legend("bottomleft", legend=rownames(TypeOfJob.Age.rel), col="black", 
      fill = c("white", "grey80", "grey40", "black"), cex=0.65)

axis(2, at=seq(0, 1, 0.2))

barplot(TypeOfJob.Age.rel, beside = TRUE)                            # grouped bar chart

# we can perform graphical comparisons between some pairs of variables (both categorical and continuous), using the plot(),  pairs() and identify() procedures

par(mfrow=c(1,2))
plot (AmountRequested, Capital, main = "Amount req. vs. Market price", 
      cex = .5, col = "dark red")

plot (log10(AmountRequested), log10(Capital+1), main = "Amount req. vs. Market price (better)", cex = .5, col = "dark red")

# adding a center (dashed) and a regression line (blue)

abline(v = mean(log10(AmountRequested)), lty = 2, col = "grey40")
abline(h = mean(log10(Capital+1)), lty = 2, col = "grey40")
abline(lm(log10(Capital+1) ~ log10(AmountRequested)), col = "blue")

# (note that log10(x+1)=0 for x=0, so our transformation keeps the zeros)
# On the other hand, these same zeros spoil the regression: perhaps it would be more sensible to do the regression without them


par(mfrow=c(1,1))

plot (TypeOfJob, AmountRequested)

plot (Age,Assessment, col="red") # WARNING!
plot (Assessment, Age, col="red") # better

plot (Assessment, TypeOfJob,col="blue", xlab="Assessment",ylab="TypeOfJob")

pairs(~ AmountRequested + Capital + Age)

# Plotting a variable against the normal pdf in red

hist.with.normal <- function (x, main, xlabel=deparse(substitute(x)), ...)
{
  h <- hist(x,plot=F, ...)
  s <- sd(x)
  m <- mean(x)
  ylim <- range(0,h$density,dnorm(0,sd=s))
  hist(x,freq=FALSE,ylim=ylim,xlab=xlabel, main=main, ...)
  curve(dnorm(x,m,s),add=T,col="red")
}

hist.with.normal (Expenses, "Expenses")

par(mfrow=c(2,4))
for (i in 0:7) 
  { plot(density(Credit.new[,i+9]), xlab="", main=names(Credit.new)[i+9]) }

# do any of the continuous variables "look" Gaussian? 
# features to look for in comparing to a Gaussian: outliers, asymmetries, long tails

# A useful tool for "Gaussianization" is the Box-Cox power transformation

library(MASS)

par(mfrow=c(1,3))

hist(Capital, main="Look at that ...")

bx <- boxcox(I(Capital+1) ~ . - Assessment, data = Credit.new,
             lambda = seq(-0.25, 0.25, length = 10))

lambda <- bx$x[which.max(bx$y)]

Capital.BC <- (Capital^lambda - 1)/lambda

hist(Capital.BC, main="Look at that now!")

par (mfrow=c(1,1))


####################################################################
# SECTION 9: ENDING THE PREPROCESSING
####################################################################
			    
# Shuffle the data (to avoid possible ordering biases)

set.seed (104)
Credit.new <- Credit.new[sample.int(nrow(Credit.new)),]
 
# Save the preprocessed data into a file for future use

# WARNING! This creates a .Rdata file (binary and gzip compressed)
# This is very convenient, but the file cannot be opened with a text editor

# If you want a text file, set 'ascii'=TRUE 

save(Credit.new, file = "Credsco-processed.Rdata")

# remove (almost) everything in the working environment
rm(list = ls())

load("Credsco-processed.Rdata")
objects()
