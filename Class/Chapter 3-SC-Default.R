
##################################################################################################################
##################################################################################################################
# Chapter 3 - Supervised classification - Default
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Open R or R-studio
##################################################################################################################

##################################################################################################################
# Control of the number of decimals in the outputs

options(digits=4)

##################################################################################################################
##################################################################################################################
# The Default data set
##################################################################################################################
##################################################################################################################

# Install and load the library ISLR, if it is not already installed

install.packages("ISLR")
library("ISLR")

# See the help page

?Default

# Load the data set into memory

data(Default)

# Check the type of object

is(Default)

# Essentially, it is a data frame. Which dimension?

dim(Default)

# Have a look a it

head(Default)

##################################################################################################################
# Split the data set in two parts

# Define the data matrix

Default.X <- Default[,2:ncol(Default)]
head(Default.X)

# Transform the factor variable into a binary variable

Default.X[,1] <- 1 * (Default.X[,1]=="Yes")
head(Default.X)

# Define the response vector

Default.Y <- Default[,1]
head(Default.Y)

# Check that the response vector is a vector but it is not a matrix

is(Default.Y)

# Obtain sample size and dimensions of the data matrix

n.Default.X <- nrow(Default.X)
n.Default.X
p.Default.X <- ncol(Default.X)
p.Default.X

# Compute number of clients in each group (1, no default, and 2, default)

n.Yes.Default <- sum(Default.Y=="Yes")
n.Yes.Default
n.No.Default <- sum(Default.Y=="No")
n.No.Default

# Proportions of clients in each class

n.Yes.Default / n.Default.X
n.No.Default / n.Default.X

# Thus, 96.67% of the clients are non-default and 3.33% are default

# Create a color object to distinguish the two groups in the plots

colors.Default.Y <- c("deepskyblue4","firebrick4")[Default.Y]
head(colors.Default.Y)

##################################################################################################################
# Make a scatterplot matrix and a parallel coordinates plot of the data matrix

pairs(Default.X,col=colors.Default.Y,pch=20)

library(MASS)

parcoord(Default.X,col=colors.Default.Y,var.label=FALSE)

# Apparently, most of the information regarding the two groups is in the balance

##################################################################################################################
##################################################################################################################
# KNN for the Default data set with training and test samples
##################################################################################################################
##################################################################################################################

# As one of the predictors in qualitative, we use the function knngow that implements the knn algorithm
# with the Gower distance

##################################################################################################################
# First, obtain the test and the training samples. For that, we take the 70% of the observations
# as training and the 30% as test
##################################################################################################################

# Fix the seed to have the same results

set.seed(1)

# Sample sizes of both data sets

n.Default.training <- floor(.7 * n.Default.X)
n.Default.training
n.Default.test <- n.Default.X - n.Default.training
n.Default.test

# Obtain the indices of the observations in both data sets

indices.Default.training <- sort(sample(1:n.Default.X,n.Default.training))
length(indices.Default.training)
head(indices.Default.training)

indices.Default.test <- setdiff(1:n.Default.X,indices.Default.training)
length(indices.Default.test)
head(indices.Default.test)

# Obtain the training data set

Default.X.training <- Default.X[indices.Default.training,]
head(Default.X.training)
Default.Y.training <- Default.Y[indices.Default.training]
head(Default.Y.training)

# Obtain the test data set

Default.X.test <- Default.X[indices.Default.test,]
head(Default.X.test)
Default.Y.test <- Default.Y[indices.Default.test]
head(Default.Y.test)

# Which are the proportions of Default and No Default clients in both data sets?

sum(Default.Y.training=="No")/n.Default.training
sum(Default.Y.training=="Yes")/n.Default.training

sum(Default.Y.test=="No")/n.Default.test
sum(Default.Y.test=="Yes")/n.Default.test

# So, they are close to the original 96.66% of no Default clients and the 3.33% of Default clients

##################################################################################################################
# Second, apply KNN and select the best K in terms of the test error
##################################################################################################################

# Install and load the library dprep, if it is not already installed

install.packages("dprep")
library("dprep")

# We take K ranging from 1 to 10

test.error.Default <- matrix(NA,nrow=10,ncol=1)
for (i in 1 : 10){
  print(i)
  knn.out <- knngow(cbind(Default.X.training,Default.Y.training),Default.X.test,k=i)
  test.error.Default[i] <- 1 - mean(knn.out==Default.Y.test)
}
plot(1:10,test.error.Default,pch=20,col="deepskyblue4",type="b")

# Take K as the one that gives the minimum test error

K <- which.min(test.error.Default)
K

# Have a look at the solution with the chosen K:

knn.out.K <- knngow(cbind(Default.X.training,Default.Y.training),Default.X.test,k=K)
head(knn.out.K)

# Number of clients classified in each group

table(knn.out.K)

# Table with good and bad classifications

table(Default.Y.test,knn.out.K)

# Obtain the test error

test.error.Default <- 1 - mean(knn.out.K==Default.Y.test)
test.error.Default

# Note that the main problem is the number of Default clients classifed as No Default clients

##################################################################################################################
##################################################################################################################
# There is no function to run KNN with cross-validation. It is necesary to implement it using the function
# knngow for every observation in the whole data matrix X. This is very time consuming, because it requires
# to do this for every value of K from 1 to 10, for instance.
# Thus, we skip it from the analysis
##################################################################################################################
##################################################################################################################

##################################################################################################################
##################################################################################################################
# Logistic regression for the Default data set with training and test samples
##################################################################################################################
##################################################################################################################

install.packages("nnet")
library(nnet)

# Fit the model with the training samples

lr.Default.training <- multinom(Default.Y.training ~ .,data=Default.X.training)

# Have a look at the estimated coefficients and their standard errors

summary(lr.Default.training)

# We can see which are the most significant coefficients with the t-test

t.Default.training <- summary(lr.Default.training)$coefficients/summary(lr.Default.training)$standard.errors
t.Default.training

# Note that some of the coefficients are not significant. 
# It is advisable to carry out a backward eliminitation procedure to discard nonsignificant variables.
# We are not going to carry out something like this here.

# Predict the class of the test sample

lr.Default.test <- predict(lr.Default.training,newdata=Default.X.test)
head(lr.Default.test)

# Number of clients classified in each group

table(lr.Default.test)

# Table with good and bad classifications

table(Default.Y.test,lr.Default.test)

# The test error is

lr.error.Default <- 1 - mean(lr.Default.test==Default.Y.test)
lr.error.Default

# Obtain the probabilities of Default

lr.Default.test.probs <- predict(lr.Default.training,newdata=Default.X.test,"probs")

# Make a plot of the errors made and the probabilities to see if they are complicated cases
# In orange, wrong classifications, in green, good classifications

colors.errors.Default <- c("orange","chartreuse")[1*(lr.Default.test==Default.Y.test)+1]
plot(1:n.Default.test,lr.Default.test.probs,col=colors.errors.Default,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Logistic regression for the Default data set with cross-validation
# The computational cost is very high
##################################################################################################################
##################################################################################################################

# Define a vector to include the classifications of each observation in the data set

lr.Default.cv.out <- matrix("NA",nrow=n.Default.X,ncol=1)
head(lr.Default.cv.out)

# Make the cross-validation procedure to obtain the fitted values

for (i in 1:n.Default.X){
  print(i)
  lr.Default.cv <- multinom(Default.Y[-i] ~ .,data=Default.X[-i,])
  lr.Default.cv.out[i] <- c("No","Yes")[predict(lr.Default.cv,newdata=Default.X[i,])]
}

# Number of emails classified in each group

table(lr.Default.cv.out)

# Table with good and bad classifications

table(Default.Y,lr.Default.cv.out)

# The test error is

lr.error.Default.cv <- 1 - mean(lr.Default.cv.out==Default.Y)
lr.error.Default.cv

##################################################################################################################
##################################################################################################################
# Linear classifier for the Default data set with training and test samples
##################################################################################################################
##################################################################################################################

# The linear, quadratic and naiveBayes classifier does not consider qualitative predictors
# Therefore, we skip the variable student from the analysis

Default.X <- Default.X[,c(2,3)]
head(Default.X)

Default.X.training <- Default.X.training[,c(2,3)]
head(Default.X.training)

Default.X.test <- Default.X.test[,c(2,3)]
head(Default.X.test)

# Linear discrimination

lda.Default.training <- lda(Default.Y.training ~ .,data=Default.X.training)

# Estimation of the prior probabilities

lda.Default.training$prior

# Predict the class of the test sample

lda.Default.test <- predict(lda.Default.training,newdata=Default.X.test)

# Have a look at the outputs

summary(lda.Default.test)

# Classifications made

head(lda.Default.test$class)

# Number of emails classified in each group

summary(lda.Default.test$class)

# Table with good and bad classifications

table(Default.Y.test,lda.Default.test$class)

# The test error is

lda.error.Default <- 1 - mean(lda.Default.test$class==Default.Y.test)
lda.error.Default

# Probabilities of the classifications made

prob.lda.test.Default <- lda.Default.test$posterior
head(prob.lda.test.Default)

# Make a plot of the probabilities of No Default
# In orange, wrong classifications, in green, good classifications

colors.lda.errors.Default <- c("orange","chartreuse")[1*(lda.Default.test$class==Default.Y.test)+1]
plot(1:n.Default.test,prob.lda.test.Default[,1],col=colors.lda.errors.Default,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Linear classifier for the Default data set with cross-validation
##################################################################################################################
##################################################################################################################

lda.Default.cv <- lda(Default.Y ~ .,data=Default.X,CV=TRUE)

# Have a look at the outputs

summary(lda.Default.cv)

# Classifications made

head(lda.Default.cv$class)

# Number of emails classified in each group

summary(lda.Default.cv$class)

# Table with good and bad classifications

table(Default.Y,lda.Default.cv$class)

# The test error is

lda.error.Default.cv <- 1 - mean(lda.Default.cv$class==Default.Y)
lda.error.Default.cv

# Probabilities of the classifications made

prob.lda.cv.Default <- lda.Default.cv$posterior
head(prob.lda.cv.Default)

# Make a plot of the errors made and the probabilities of No Default to see if they are complicated cases
# In orange, wrong classifications, in green, good classifications

colors.lda.cv.errors.Default <- c("orange","chartreuse")[1*(lda.Default.cv$class==Default.Y)+1]
plot(1:n.Default.X,prob.lda.cv.Default[,1],col=colors.lda.cv.errors.Default,pch=20,type="p",xlab="Sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Quadratic classifier for the Default data set with training and test samples
##################################################################################################################
##################################################################################################################

qda.Default.training <- qda(Default.Y.training ~ .,data=Default.X.training)

# Estimation of the prior probabilities

qda.Default.training$prior

# Predict the class of the test sample

qda.Default.test <- predict(qda.Default.training,newdata=Default.X.test[,-41])

# Have a look at the outputs

summary(qda.Default.test)

# Classifications made

head(qda.Default.test$class)

# Number of emails classified in each group

summary(qda.Default.test$class)

# Table with good and bad classifications

table(Default.Y.test,qda.Default.test$class)

# The test error is

qda.error.Default <- 1 - mean(qda.Default.test$class==Default.Y.test)
qda.error.Default

# Probabilities of the classifications made

prob.qda.test.Default <- qda.Default.test$posterior
head(prob.qda.test.Default)

# Make a plot of the probabilities of No Default
# In orange, wrong classifications, in green, good classifications

colors.qda.errors.Default <- c("orange","chartreuse")[1*(qda.Default.test$class==Default.Y.test)+1]
plot(1:n.Default.test,prob.qda.test.Default[,1],col=colors.qda.errors.Default,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Quadratic classifier for the Default data set with cross-validation
##################################################################################################################
##################################################################################################################

qda.Default.cv <- qda(Default.Y ~ .,data=Default.X,CV=TRUE)

# Have a look at the outputs

summary(qda.Default.cv)

# Classifications made

head(qda.Default.cv$class)

# Number of emails classified in each group

summary(qda.Default.cv$class)

# Table with good and bad classifications

table(Default.Y,qda.Default.cv$class)

# The test error is

qda.error.Default.cv <- 1 - mean(qda.Default.cv$class==Default.Y,na.rm=TRUE)
qda.error.Default.cv

# Probabilities of the classifications made

prob.qda.cv.Default <- qda.Default.cv$posterior
head(prob.qda.cv.Default)

# Make a plot of the errors made and the probabilities of No Default to see if they are complicated cases
# In orange, wrong classifications, in green, good classifications

colors.qda.cv.errors.Default <- c("orange","chartreuse")[1*(qda.Default.cv$class==Default.Y)+1]
plot(1:n.Default.X,prob.qda.cv.Default[,1],col=colors.qda.cv.errors.Default,pch=20,type="p",xlab="Sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Naive Bayes classifier for the Default data set with training and test samples
##################################################################################################################
##################################################################################################################

# Install and load the library e1071 if it is not already installed

install.packages("e1071")
library("e1071")

# Naive Bayes

nb.Default.training <- naiveBayes(Default.Y.training ~ .,data=Default.X.training)

# Estimation of the prior probabilities

nb.Default.training$apriori

# Predict the class of the test sample

nb.Default.test <- predict(nb.Default.training,newdata=Default.X.test)

# Classifications made

head(nb.Default.test)

# Number of emails classified in each group

summary(nb.Default.test)

# Table with good and bad classifications

table(Default.Y.test,nb.Default.test)

# The test error is

nb.error.Default <- 1 - mean(nb.Default.test==Default.Y.test)
nb.error.Default

# Probabilities of the classifications made

prob.nb.Default.test <- predict(nb.Default.training,newdata=Default.X.test,type ="raw")
head(prob.nb.Default.test)

# Make a plot of the probabilities of No Default
# In orange, wrong classifications, in green, good classifications

colors.nb.errors.Default <- c("orange","chartreuse")[1*(nb.Default.test==Default.Y.test)+1]
plot(1:n.Default.test,prob.nb.Default.test[,1],col=colors.nb.errors.Default,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Naive Bayes classifier for the Default data set with cross-validation
# Takes a few minutes
##################################################################################################################
##################################################################################################################

nb.Default.cv.out <- matrix(NA,nrow=n.Default.X,ncol=1)
for (i in 1:n.Default.X){
  print(i)
  nb.Default.cv <- naiveBayes(as.factor(Default.Y[-i]) ~ .,data=Default.X[-i,])  
  nb.Default.cv.out[i] <- c("No","Yes")[predict(nb.Default.cv,Default.X[i,])]
}

# Number of emails classified in each group

table(nb.Default.cv.out)

# Table with good and bad classifications

table(Default.Y,nb.Default.cv.out)

# The test error is

nb.error.Default.cv <- 1 - mean(nb.Default.cv.out==Default.Y,na.rm=TRUE)
nb.error.Default.cv
