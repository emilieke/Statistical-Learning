
##################################################################################################################
##################################################################################################################
# Chapter 3 - Supervised classification
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
# Control of the number of decimals in the outputs

options(digits=4)

##################################################################################################################
##################################################################################################################
# The Spam data set
##################################################################################################################
##################################################################################################################

# Install and load the library kernlab, if it is not already installed

install.packages("kernlab")
library("kernlab")

# See the help page

?spam

# Load the data set into memory

data(spam)

# Check the type of object

is(spam)

# Essentially, it is a data frame. Which dimension?

dim(spam)

# Have a look a it

head(spam)

##################################################################################################################
# Split the data set in two parts

# Define the data matrix and the response vector

spam.X <- spam[,1:(ncol(spam)-1)]
head(spam.X)

spam.Y <- spam[,ncol(spam)]
head(spam.Y)

# Check that the response vector is a vector but it is not a matrix

is(spam.Y)

# Obtain sample size and dimensions of the data matrix

n.spam.X <- nrow(spam.X)
n.spam.X
p.spam.X <- ncol(spam.X)
p.spam.X

# Compute number of clients in each group

n.nonspam <- sum(spam.Y=="nonspam")
n.nonspam
n.spam <- sum(spam.Y=="spam")
n.spam

# Proportions of clients in each class

n.nonspam / n.spam.X
n.spam / n.spam.X

# Thus, 60.6% of the emails are non-spam and 39.4% are spam

# Create a color object to distinguish the two groups in the plots

colors.spam.Y <- c("deepskyblue4","firebrick4")[spam.Y]
head(colors.spam.Y)

##################################################################################################################
# Make a parallel coordinates plot of the data matrix

library(MASS)

parcoord(spam.X,col=colors.spam.Y,var.label=FALSE)

# It is a complicated problem because the two groups are very mixed

##################################################################################################################
# PCA for the spam data set to try to see if the groups are better seen in the PCS

PCS.spam <- prcomp(spam.X,scale=TRUE)
summary(PCS.spam)

# A very large number of PCs (28) are needed to explain a 70% of the total variability

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.spam$x[,1:2],pch=20,col=colors.spam.Y)

# This plot suggests that there migth be some options to distinguish the two groups

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
# Make a parallel coordinates plot of the data matrix

pairs(Default.X,col=colors.Default.Y,pch=20)
parcoord(Default.X,col=colors.Default.Y,var.label=FALSE)

# Apparently, most of the information regarding the two groups is in the balance

##################################################################################################################
##################################################################################################################
# KNN for the Spam data set with training and test samples
##################################################################################################################
##################################################################################################################

# There are many different implementations in R of the KNN procedure

# The most popular one is the function knn from library class that performs KNN with a training and
# a test sample with fixed K

##################################################################################################################
# First, obtain the test and the training samples. For that, we take the 70% of the observations
# as training and the 30% as test
##################################################################################################################

# Fix the seed to have the same results

set.seed(1)

# Sample sizes of both data sets

n.spam.training <- floor(.7 * n.spam.X)
n.spam.training
n.spam.test <- n.spam.X - n.spam.training
n.spam.test

# Obtain the indices of the observations in both data sets

indices.spam.training <- sort(sample(1:n.spam.X,n.spam.training))
length(indices.spam.training)
head(indices.spam.training)

indices.spam.test <- setdiff(1:n.spam.X,indices.spam.training)
length(indices.spam.test)
head(indices.spam.test)

# Obtain the training data set

spam.X.training <- spam.X[indices.spam.training,]
head(spam.X.training)
spam.Y.training <- spam.Y[indices.spam.training]
head(spam.Y.training)

# Obtain the test data set

spam.X.test <- spam.X[indices.spam.test,]
head(spam.X.test)
spam.Y.test <- spam.Y[indices.spam.test]
head(spam.Y.test)

# Which are the proportions of spam and non-spam emails in both data sets?

sum(spam.Y.training=="nonspam")/n.spam.training
sum(spam.Y.training=="spam")/n.spam.training

sum(spam.Y.test=="nonspam")/n.spam.test
sum(spam.Y.test=="spam")/n.spam.test

# So, they are close to the original 60.6% of the non-spam emails are non-spam and 39.4% are spam

##################################################################################################################
# Second, apply KNN and select the best K in terms of the test error
##################################################################################################################

# Install and load the library class, if it is not already installed

install.packages("class")
library("class")

# The function knn uses the Euclidean distance. Therefore, we standarize both data sets.

stan.spam.X.traning <- scale(spam.X.training)
stan.spam.X.test <- scale(spam.X.test)

# We take K ranging from 1 to 50 as the sample size is big enough

test.error.spam <- matrix(NA,nrow=50,ncol=1)
for (i in 1 : 50){
  print(i)
  knn.out <- knn(stan.spam.X.traning,stan.spam.X.test,spam.Y.training,k=i)
  test.error.spam[i] <- 1 - mean(knn.out==spam.Y.test)
}
plot(1:50,test.error.spam,pch=20,col="deepskyblue4",type="b")

# Take K as the one that gives the minimum test error

K <- which.min(test.error.spam)
K

# Have a look at the solution with the chosen K. Note that now the function also return
# the estimated probabilities as an attribute

knn.out.K <- knn(stan.spam.X.traning,stan.spam.X.test,spam.Y.training,k=K,prob=TRUE)
head(knn.out.K)

# Number of emails classified in each group

summary(knn.out.K)

# Table with good and bad classifications

table(spam.Y.test,knn.out.K)

# Obtain the test error

test.error.spam <- 1 - mean(knn.out.K==spam.Y.test)
test.error.spam

# Probabilities of the classifications made

prob.test.spam <- attributes(knn.out.K)$prob
head(prob.test.spam)

# Note that only the probabilities of the winner group are shown
# This happens if we have only two groups. For more groups the function
# gives the probabilities for all the groups

# Make a plot of the probabilities of the winner group
# In orange, wrong classifications, in green, good classifications

colors.errors.spam <- c("orange","chartreuse")[1*(knn.out.K==spam.Y.test)+1]
plot(1:n.spam.test,prob.test.spam,col=colors.errors.spam,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

# See that most of the errors are associated with the lower probabilities

##################################################################################################################
##################################################################################################################
# KNN for the Spam data set with cross-validation
##################################################################################################################
##################################################################################################################

# The function knn.cv uses the Euclidean distance. Therefore, we standarize the data set

stan.spam.X <- scale(spam.X)

# We take K ranging from 1 to 20 as the sample size is big enough

cv.error.spam <- matrix(NA,nrow=20,ncol=1)
for (i in 1 : 20){
  print(i)
  knn.cv.out <- knn.cv(stan.spam.X,spam.Y,k=i)
  cv.error.spam[i] <- 1 - mean(knn.cv.out==spam.Y)
}
plot(1:20,cv.error.spam,pch=20,col="deepskyblue4",type="b")

# Take K as the one that gives the minimum test error

K <- which.min(cv.error.spam)
K

# In this case K=1 is selected. Note that this can be somehow extreme and might be better 
# consider another option

# Have a look at the solution with the chosen K. Note that now the function also return
# the estimated probabilities

knn.cv.out.K <- knn.cv(stan.spam.X,spam.Y,k=K,prob=TRUE)
head(knn.cv.out.K)

# Number of emails classified in each group

summary(knn.cv.out.K)

# Table with good and bad classifications

table(spam.Y,knn.cv.out.K)

# Obtain the test error

cv.error.spam <- 1 - mean(knn.cv.out.K==spam.Y)
cv.error.spam

# Probabilities of the classifications made

prob.cv.spam <- attributes(knn.cv.out.K)$prob
head(prob.cv.spam)

# Note that only the probabilities of the winner group are shown
# This happens if we have only two groups. For more groups the function
# gives the probabilities for all the groups

# Make a plot of the probabilities of the winner group
# In orange, wrong classifications, in green, good classifications

colors.cv.spam <- c("orange","chartreuse")[1*(knn.cv.out.K==spam.Y)+1]
plot(1:n.spam.X,prob.cv.spam,col=colors.cv.spam,pch=20,type="p",xlab="Sample",ylab="Probabilities")

# See that all the estimated probabilities are 1 or 0.5 because K=1

##################################################################################################################
##################################################################################################################
# Logistic regression for the Spam data set with training and test samples
##################################################################################################################
##################################################################################################################

install.packages("nnet")
library(nnet)

# Fit the model with the training samples

lr.spam.training <- multinom(spam.Y.training ~ .,data=spam.X.training)

# Have a look at the estimated coefficients and their standard errors

summary(lr.spam.training)

# We can see which are the most significant coefficients with the t-test

t.spam.training <- summary(lr.spam.training)$coefficients/summary(lr.spam.training)$standard.errors
t.spam.training

# Note that some of the coefficients are not significant. 
# It is advisable to carry out a backward eliminitation procedure to discard nonsignificant variables.
# We are not going to carry out something like this here.

# Predict the class of the test sample

lr.spam.test <- predict(lr.spam.training,newdata=spam.X.test)
head(lr.spam.test)

# Number of emails classified in each group

table(lr.spam.test)

# Table with good and bad classifications

table(spam.Y.test,lr.spam.test)

# The test error is

lr.error.spam <- 1 - mean(lr.spam.test==spam.Y.test)
lr.error.spam

# Obtain the probabilities of the first level which in this case is nonspam

lr.spam.test.probs <- predict(lr.spam.training,newdata=spam.X.test,"probs")

# Make a plot of the errors made and the probabilities to see if they are complicated cases
# In orange, wrong classifications, in green, good classifications

colors.errors.spam <- c("orange","chartreuse")[1*(lr.spam.test==spam.Y.test)+1]
plot(1:n.spam.test,lr.spam.test.probs,col=colors.errors.spam,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Logistic regression for the Spam data set with cross-validation
# The computational cost is high, thus not run in class
##################################################################################################################
##################################################################################################################

# Define a vector to include the classifications of each observation in the data set

lr.spam.cv.out <- matrix("NA",nrow=n.spam.X,ncol=1)
head(lr.spam.cv.out)

# Make the cross-validation procedure to obtain the fitted values

for (i in 1:n.spam.X){
  print(i)
  lr.spam.cv <- multinom(spam.Y[-i] ~ .,data=spam.X[-i,])
  lr.spam.cv.out[i] <- c("nonspam","spam")[predict(lr.spam.cv,newdata=spam.X[i,])]
}

# Number of emails classified in each group

table(lr.spam.cv.out)

# Table with good and bad classifications

table(spam.Y,lr.spam.cv.out)

# The test error is

lr.error.spam.cv <- 1 - mean(lr.spam.cv.out==spam.Y)
lr.error.spam.cv

##################################################################################################################
##################################################################################################################
# Linear classifier for the Spam data set with training and test samples
##################################################################################################################
##################################################################################################################

lda.spam.training <- lda(spam.Y.training ~ .,data=spam.X.training)

# Estimation of the prior probabilities

lda.spam.training$prior

# Predict the class of the test sample

lda.spam.test <- predict(lda.spam.training,newdata=spam.X.test)

# Have a look at the outputs

summary(lda.spam.test)

# Classifications made

head(lda.spam.test$class)

# Number of emails classified in each group

summary(lda.spam.test$class)

# Table with good and bad classifications

table(spam.Y.test,lda.spam.test$class)

# The test error is

lda.error.spam <- 1 - mean(lda.spam.test$class==spam.Y.test)
lda.error.spam

# Probabilities of the classifications made

prob.lda.test.spam <- lda.spam.test$posterior
head(prob.lda.test.spam)

# Make a plot of the probabilities of nonspam
# In orange, wrong classifications, in green, good classifications

colors.lda.errors.spam <- c("orange","chartreuse")[1*(lda.spam.test$class==spam.Y.test)+1]
plot(1:n.spam.test,prob.lda.test.spam[,1],col=colors.lda.errors.spam,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Linear classifier for the Spam data set with cross-validation
##################################################################################################################
##################################################################################################################

lda.spam.cv <- lda(spam.Y ~ .,data=spam.X,CV=TRUE)

# Have a look at the outputs

summary(lda.spam.cv)

# Classifications made

head(lda.spam.cv$class)

# Number of emails classified in each group

summary(lda.spam.cv$class)

# Table with good and bad classifications

table(spam.Y,lda.spam.cv$class)

# The test error is

lda.error.spam.cv <- 1 - mean(lda.spam.cv$class==spam.Y)
lda.error.spam.cv

# Probabilities of the classifications made

prob.lda.cv.spam <- lda.spam.cv$posterior
head(prob.lda.cv.spam)

# Make a plot of the errors made and the probabilities of nonspam to see if they are complicated cases
# In orange, wrong classifications, in green, good classifications

colors.lda.cv.errors.spam <- c("orange","chartreuse")[1*(lda.spam.cv$class==spam.Y)+1]
plot(1:n.spam.X,prob.lda.cv.spam[,1],col=colors.lda.cv.errors.spam,pch=20,type="p",xlab="Sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Quadratic classifier for the Spam data set with training and test samples
##################################################################################################################
##################################################################################################################

qda.spam.training <- qda(spam.Y.training ~ .,data=spam.X.training)

# You can see that there is a problem because the rank of the sample covariance matrix 
# of one of the groups is almost singular

# Obtain the eigenvalues of the sample covariance matrix of the members of the training
# sample in the spam group

eigen(cov(spam.X.training[spam.Y.training=="spam",]))$values

# You can see that there is one eigenvalue of order 10^{-19}

# There are several possibilities why this is happening
# In this case, it is because one of the columns (called cs) is exactly 0

summary(spam.X.training[spam.Y.training=="spam",])

# Therefore, we apply qda excluding this columns of all the data sets

spam.X.training[spam.Y.training=="spam",]$cs
which(colnames(spam.X.training)=="cs")

qda.spam.training <- qda(spam.Y.training ~ .,data=spam.X.training[,-41])

# Estimation of the prior probabilities

qda.spam.training$prior

# Predict the class of the test sample

qda.spam.test <- predict(qda.spam.training,newdata=spam.X.test[,-41])

# Have a look at the outputs

summary(qda.spam.test)

# Classifications made

head(qda.spam.test$class)

# Number of emails classified in each group

summary(qda.spam.test$class)

# Table with good and bad classifications

table(spam.Y.test,qda.spam.test$class)

# The test error is

qda.error.spam <- 1 - mean(qda.spam.test$class==spam.Y.test)
qda.error.spam

# Probabilities of the classifications made

prob.qda.test.spam <- qda.spam.test$posterior
head(prob.qda.test.spam)

# Make a plot of the probabilities of nonspam
# In orange, wrong classifications, in green, good classifications

colors.qda.errors.spam <- c("orange","chartreuse")[1*(qda.spam.test$class==spam.Y.test)+1]
plot(1:n.spam.test,prob.qda.test.spam[,1],col=colors.qda.errors.spam,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Quadratic classifier for the Spam data set with cross-validation
##################################################################################################################
##################################################################################################################

qda.spam.cv <- qda(spam.Y ~ .,data=spam.X,CV=TRUE)

# Have a look at the outputs

summary(qda.spam.cv)

# Classifications made

head(qda.spam.cv$class)

# Number of emails classified in each group

summary(qda.spam.cv$class)

# Table with good and bad classifications

table(spam.Y,qda.spam.cv$class)

# The test error is

qda.error.spam.cv <- 1 - mean(qda.spam.cv$class==spam.Y,na.rm=TRUE)
qda.error.spam.cv

# Probabilities of the classifications made

prob.qda.cv.spam <- qda.spam.cv$posterior
head(prob.qda.cv.spam)

# Make a plot of the errors made and the probabilities of nonspam to see if they are complicated cases
# In orange, wrong classifications, in green, good classifications

colors.qda.cv.errors.spam <- c("orange","chartreuse")[1*(qda.spam.cv$class==spam.Y)+1]
plot(1:n.spam.X,prob.qda.cv.spam[,1],col=colors.qda.cv.errors.spam,pch=20,type="p",xlab="Sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Naive Bayes classifier for the Spam data set with training and test samples
##################################################################################################################
##################################################################################################################

# Install and load the library e1071 if it is not already installed

install.packages("e1071")
library("e1071")

# Naive Bayes

nb.spam.training <- naiveBayes(spam.Y.training ~ .,data=spam.X.training)

# Estimation of the prior probabilities

nb.spam.training$apriori

# Predict the class of the test sample

nb.spam.test <- predict(nb.spam.training,newdata=spam.X.test)

# Classifications made

head(nb.spam.test)

# Number of emails classified in each group

summary(nb.spam.test)

# Table with good and bad classifications

table(spam.Y.test,nb.spam.test)

# The test error is

nb.error.spam <- 1 - mean(nb.spam.test==spam.Y.test)
nb.error.spam

# Probabilities of the classifications made

prob.nb.spam.test <- predict(nb.spam.training,newdata=spam.X.test,type ="raw")
head(prob.nb.spam.test)

# Make a plot of the probabilities of nonspam
# In orange, wrong classifications, in green, good classifications

colors.nb.errors.spam <- c("orange","chartreuse")[1*(nb.spam.test==spam.Y.test)+1]
plot(1:n.spam.test,prob.nb.spam.test[,1],col=colors.nb.errors.spam,pch=20,type="p",xlab="Test sample",ylab="Probabilities")

##################################################################################################################
##################################################################################################################
# Naive Bayes classifier for the Spam data set with cross-validation
# The computational cost is high, thus not run in class
##################################################################################################################
##################################################################################################################

nb.spam.cv.out <- matrix(NA,nrow=n.spam.X,ncol=1)
for (i in 1:n.spam.X){
  print(i)
  nb.spam.cv <- naiveBayes(as.factor(spam.Y[-i]) ~ .,data=spam.X[-i,])  
  nb.spam.cv.out[i] <- c("nonspam","spam")[predict(nb.spam.cv,spam.X[i,])]
}

# Number of emails classified in each group

table(nb.spam.cv.out)

# Table with good and bad classifications

table(spam.Y,nb.spam.cv.out)

# The test error is

nb.error.spam.cv <- 1 - mean(nb.spam.cv.out==spam.Y,na.rm=TRUE)
nb.error.spam.cv
