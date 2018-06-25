
##################################################################################################################
##################################################################################################################
# Chapter 1 - Multidimensional data - Second day
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
# Examples of well structured data sets
##################################################################################################################

##################################################################################################################
# The Spam data set

# Load the library kernlab, if it is not already installed

library("kernlab")

# See the help page

?spam

# Load the data set into memory

data(spam)

# Essentially, it is a data frame. Which dimension?

dim(spam)

# Have a look a it

head(spam)

##################################################################################################################
# The NCI60 data set

# Load the library ISLR, if it is not already installed

library("ISLR")

# See the help page

?NCI60

# Load the data set into memory

data(NCI60)

# NCI60$data is a matrix. Which dimension?

dim(NCI60$data)

# NCI60$labs is a vector of characters. Have a look at it

NCI60$labs

##################################################################################################################
# The births2006 data set

# Load the library nutshell, if it is not already installed

library("nutshell")

# See the help page

?births2006.smpl

# Load the data set into memory

data(births2006.smpl)

# t is a data frame. Which dimension?

dim(births2006.smpl)

# Have a look a it

head(births2006.smpl)

# Note the existence of missing values marked as NA

##################################################################################################################
# Sample sizes and dimensions of the Spam, NCI60 and births2006.smpl data sets
##################################################################################################################

##################################################################################################################
# Sample sizes and dimensions of the Spam data set

# Remember the structure of the data set

head(spam)

# The last column is the response vector. How many columns do we have?

ncol(spam)

# Define the data matrix and the response vector

spam.X <- spam[,1:(ncol(spam)-1)]
head(spam.X)
spam.Y <- spam[,ncol(spam)]
head(spam.Y)

# Obtain sample size and dimensions of the data matrix

n.spam <- nrow(spam.X)
n.spam
p.spam <- ncol(spam.X)
p.spam

# Check that the response vector is a vector but it is not a matrix

is(spam.Y)

# Have a look at it and see that there are two different classes

head(spam.Y)

# How many observations?

length(spam.Y)

##################################################################################################################
# Sample sizes and dimensions of the NCI60 data set

n.NCI60 <- dim(NCI60$data)[1]
n.NCI60
p.NCI60 <- dim(NCI60$data)[2]
p.NCI60

##################################################################################################################
# Sample sizes and dimensions of the births2006.smpl data set

n.NCI60 <- dim(births2006.smpl)[1]
n.NCI60
p.NCI60 <- dim(births2006.smpl)[2]
p.NCI60

##################################################################################################################
# Plots for qualitative variables
##################################################################################################################

##################################################################################################################
# Barplots and piecharts of the variable spam of the spam data set

# Obtain absolute frequencies

abs.freq.spam.Y <- table(spam.Y)
abs.freq.spam.Y

# Create a graphical window with two parts

par(mfrow=c(1,2))

# Barplot and piechart of the spam variable of the spam data set

barplot(abs.freq.spam.Y,col="deepskyblue4",main="Barplot of the variable spam",ylab="Absolute frequencies",xlab="Spam values")
pie(abs.freq.spam.Y,col=c("deepskyblue4","firebrick4"),main="Piechart of the variable spam",radius=2)

##################################################################################################################
# Barplots and piecharts of the variable DMEDUC of the births2006 data set

# Obtain absolute frequencies

abs.freq.births2006.DMEDUC <- table(births2006.smpl$DMEDUC)
abs.freq.births2006.DMEDUC

# Create a graphical window with two parts

par(mfrow=c(1,2))

# Barplot and piechart of the DMEDUC variable of the births2006 data set

barplot(abs.freq.births2006.DMEDUC,col="deepskyblue4",main="Barplot of the variable DMEDUC",ylab="Absolute frequencies",xlab="DMEDUC values")
pie(abs.freq.births2006.DMEDUC,main="Piechart of the variable DMEDUC",radius=2)

##################################################################################################################
# Barplot for the variables SEX abd DMEDUC of the births2006 data set

# Create a graphical window with only one part

par(mfrow=c(1,1))

plot(births2006.smpl$DMEDUC,births2006.smpl$SEX,xlab="DMEDUC",ylab="SEX",col=c("deepskyblue4","firebrick4"))

##################################################################################################################
# Plots for quantitative variables
##################################################################################################################

##################################################################################################################
# Barplot of the variable capitalLong of the spam data set

# Obtain absolute frequencies

abs.freq.spam.capitalLong <- table(spam.X$capitalLong)
abs.freq.spam.capitalLong

# Barplot of the variable capitalLong of the spam data set

barplot(abs.freq.spam.capitalLong,col="deepskyblue4",main="Barplot of the variable capitalLong",ylab="Absolute frequencies",xlab="capitalLong values")

##################################################################################################################
# Histograms of the first 10 variables of the NCI60 data set

# Create a graphical window with ten parts (2 rows and 5 columns)

par(mfrow=c(2,5))

# Plot the 10 boxplots

sapply(seq(1,10),function(j)boxplot(NCI60$data[,j],main=colnames(NCI60$data)[j],xlab="",col="deepskyblue4"))

##################################################################################################################
# Boxplot of the variable capitalAve of the spam data set

# Create a graphical window with only one part

par(mfrow=c(1,1))

# Plot the boxplot

boxplot(spam.X$capitalAve,main="Boxplot of capitalAve",xlab="",col="deepskyblue4")

# Show high skewness. Then, transform with logarithm

boxplot(log(spam.X$capitalAve),main="Boxplot of logarithm of capitalAve",xlab="",col="deepskyblue4")

# Plot boxplots of the logarithm of capitalAve in terms of the variable spam

boxplot(log(spam.X$capitalAve)~spam.Y,main="Boxplot of logarithm of capitalAve",xlab="",ylab="Logarithm of capitalAve",col="deepskyblue4")

##################################################################################################################
# Histograms of the first 10 variables of the NCI60 data set

# Create a graphical window with ten parts (2 rows and 5 columns)

par(mfrow=c(2,5))

# Plot the 10 histograms

sapply(seq(1,10),function(j)hist(NCI60$data[,j],main=colnames(NCI60$data)[j],xlab="",freq=FALSE,col="deepskyblue4"))

##################################################################################################################
# Histogram of the variable capitalAve of the spam data set

# Create a graphical window with only one part

par(mfrow=c(1,1))

# Plot the histogram

hist(spam.X$capitalAve,main="Histogram of capitalAve",xlab="",col="deepskyblue4",freq=FALSE)

# Transform with logarithm

hist(log(spam.X$capitalAve),main="Boxplot of logarithm of capitalAve",xlab="",col="deepskyblue4",freq=FALSE)

# Plot histograms of the logarithm of capitalAve in terms of the variable spam

# Create two histogram objects

hist.spam <- hist(log(spam.X$capitalAve[spam.Y=="spam"]),plot=FALSE)
hist.nonspam <- hist(log(spam.X$capitalAve[spam.Y=="nonspam"]),plot=FALSE)

# Plot the histograms together

plot(hist.spam,col=rgb(0,0,1,1/4),xlim=c(0,max(log(spam.X$capitalAve))),ylim=c(0,1),freq=FALSE,xlab="",main="Histogram of capitalAve in terms of spam")
plot(hist.nonspam,col=rgb(1,0,0,1/4),xlim=c(0,max(log(spam.X$capitalAve))),freq=FALSE,add=T)

##################################################################################################################
# Kernel densities of the first 10 variables of the NCI60 data set

# Create a graphical window with ten parts (2 rows and 5 columns)

par(mfrow=c(2,5))

# Plot the 10 Gaussian kernel densities

sapply(seq(1,10),function(j)plot(density(NCI60$data[,j],kernel="gaussian"),main=colnames(NCI60$data)[j],xlab="",col="deepskyblue4",lwd=2))

##################################################################################################################
# Kernel density of the variable capitalAve of the spam data set

# Create a graphical window with only one part

par(mfrow=c(1,1))

# Plot the histogram

plot(density(spam.X$capitalAve,kernel="gaussian"),main="Kernel density of capitalAve",xlab="",col="deepskyblue4",lwd=2)

# Transform with logarithm

plot(density(log(spam.X$capitalAve),kernel="gaussian"),main="Kernel density of logarithm of capitalAve",xlab="",col="deepskyblue4",lwd=2)

# Plot kernel densities of the logarithm of capitalAve in terms of the variable spam

d.spam <- density(log(spam.X$capitalAve[spam.Y=="spam"]),kernel="gaussian")
min.x.d.spam <- min(d.spam$x)
max.x.d.spam <- max(d.spam$x)
min.y.d.spam <- min(d.spam$y)
max.y.d.spam <- max(d.spam$y)

d.nonspam <- density(log(spam.X$capitalAve[spam.Y=="nonspam"]),kernel="gaussian")
min.x.d.nonspam <- min(d.nonspam$x)
max.x.d.nonspam <- max(d.nonspam$x)
min.y.d.nonspam <- min(d.nonspam$y)
max.y.d.nonspam <- max(d.nonspam$y)

min.x <- min(c(min.x.d.spam,min.x.d.nonspam))
max.x <- max(c(max.x.d.spam,max.x.d.nonspam))
min.y <- min(c(min.y.d.spam,min.y.d.nonspam))
max.y <- max(c(max.y.d.spam,max.y.d.nonspam))

plot(c(min.x,max.x),c(min.y,max.y),xlab="",ylab="",main="Kernel density of logarithm of capitalAve in terms of spam",type="n")
lines(d.spam$x,d.spam$y,col="deepskyblue4",lwd=2)
lines(d.nonspam$x,d.nonspam$y,col="firebrick4",lwd=2)

##################################################################################################################
# Scatterplot of the two first variables in the NCI60 data set

plot(NCI60$data[,1],NCI60$data[,2],pch=20,col="deepskyblue4",xlab="First variable",ylab="Second variable",main="Scatterplot")

##################################################################################################################
# 3D-Scatterplots of the three first variables in the NCI60 data set

# Install and load library scatterplot3d

install.packages("scatterplot3d")
library("scatterplot3d")

# Three-variate scatterplot

scatterplot3d(NCI60$data[,1:3],pch=20,color="deepskyblue4",main="3D Scatterplot")
scatterplot3d(NCI60$data[,1:3],pch=20,color="deepskyblue4",main="3D Scatterplot",type="h")

# Another way. Install and load library rgl

install.packages("rgl")
library("rgl")

# Open a graphical window in three dimensions

open3d() 

# Three-variate scatterplot

plot3d(NCI60$data[,1:3],size=5,col="deepskyblue4")

##################################################################################################################
# Scatterplot matrix for the ten first variables in the NCI60 data set

pairs(NCI60$data[,1:10],pch=20,col="deepskyblue4")

##################################################################################################################
# Parallel coordinates plots for the ten first variables in the NCI60 data set

# Install and load library MASS

install.packages("MASS")
library("MASS")

parcoord(NCI60$data[,1:10],col="deepskyblue4",var.label=TRUE)

# Increase the number of variables

parcoord(NCI60$data[,1:20],col="deepskyblue4",var.label=TRUE)

parcoord(NCI60$data[,1:100],col="deepskyblue4",var.label=TRUE)

##################################################################################################################
# Sample mean vector
##################################################################################################################

##################################################################################################################
# Sample mean vector for the spam data set

smv.spam.X <- colMeans(spam.X)
smv.spam.X

##################################################################################################################
# Sample covariance matrix
##################################################################################################################

##################################################################################################################
# Sample covariance matrix for the spam data set

S.spam.X <- cov(spam.X)
S.spam.X
dim(S.spam.X)

# Obtain eigenvectors and eigenvalues of the sample covariance matrix

eig.S.spam.X <- eigen(S.spam.X)

eig.S.spam.X$vectors
dim(eig.S.spam.X$vectors)

eig.S.spam.X$values

# Plot the eigenvalues of the sample covariance matrix

plot(1:p.spam,eig.S.spam.X$values,col="deepskyblue4",type="b",xlab="Number",ylab="Eigenvalues",pch=19)

##################################################################################################################
# For large dimensions, cov can be very slow. It is better to use the function cov.shrink from library
# corpcor

install.packages("corpcor")
library("corpcor")

# Example with the NCI60 data set

S.NCI60 <- cov.shrink(NCI60$data,lambda=0,lambda.var=0,verbose=FALSE)
S.NCI60[1:10,1:10]

##################################################################################################################
# Sample correlation matrix
##################################################################################################################

##################################################################################################################
# Sample correlation matrix for the spam data set

R.spam.X <- cor(spam.X)
R.spam.X
dim(R.spam.X)

# Obtain a plot of the correlation matrix

install.packages("corrplot")
library("corrplot")

corrplot(R.spam.X)
corrplot(R.spam.X,order="hclust")

# Obtain eigenvectors and eigenvalues of the sample correlation matrix

eig.R.spam.X <- eigen(R.spam.X)

eig.R.spam.X$vectors
dim(eig.R.spam.X$vectors)

eig.R.spam.X$values

# Plot the eigenvalues of the sample correlation matrix

plot(1:p.spam,eig.R.spam.X$values,col="deepskyblue4",type="b",xlab="Number",ylab="Eigenvalues",pch=19)

##################################################################################################################
# Compute the sample correlation matrix from the sample covariance matrix for the NCI data set

R.NCI60 <- cov2cor(S.NCI60)
R.NCI60[1:10,1:10]

##################################################################################################################
# Standardization of the spam data set
##################################################################################################################

# Standardize the data

stan.spam.X <- scale(spam.X)
stan.spam.X
dim(stan.spam.X)

# Obtain covariance and correlation matrix of the standardized data

S.stan.spam.X <- cov(stan.spam.X)
R.stan.spam.X <- cor(stan.spam.X)

# Show first five rows and columns

S.stan.spam.X[1:5,1:5]
R.stan.spam.X[1:5,1:5]

##################################################################################################################
# Mahalanobis distances for the College data set
##################################################################################################################

# See the help page

?College

# Load the data set into memory

data(College)

# Check the type of object

is(College)

# Essentially, it is a data frame. Which dimension?

dim(College)

# Have a look a it

head(College)

# We have two groups in the data: private and nonprivate schools

# Split the data set in two groups

College.priv <- College[College$Private=="Yes",]
head(College.priv)

College.nonpriv <- College[College$Private=="No",]
head(College.nonpriv)

# Eliminate the variable Private in both groups

College.priv <- College.priv[,2:ncol(College.priv)]
head(College.priv)

College.nonpriv <- College.nonpriv[,2:ncol(College.nonpriv)]
head(College.nonpriv)

##################################################################################################################
# Obtain the (squared) Mahalanobis distances with respect to the sample mean vector
# for the private colleges

smv.College.priv <- colMeans(College.priv)
smv.College.priv

S.College.priv <- cov(College.priv)
S.College.priv

mah.College.priv <- mahalanobis(College.priv,smv.College.priv,S.College.priv)
mah.College.priv

# Sort the Mahalanobis distances

sort.mah.College.priv <- sort(mah.College.priv,index.return=TRUE)$x
sort.mah.College.priv

# Plot the sorted distances

plot(sort.mah.College.priv,pch=19,col="deepskyblue",xlab="",ylab="",main="Mahalanobis distances for the private Colleges")

##################################################################################################################
# Obtain the (squared) Mahalanobis distances with respect to the sample mean vector
# for the nonprivate colleges

smv.College.nonpriv <- colMeans(College.nonpriv)
smv.College.nonpriv

S.College.nonpriv <- cov(College.nonpriv)
S.College.nonpriv

mah.College.nonpriv <- mahalanobis(College.nonpriv,smv.College.nonpriv,S.College.nonpriv)
mah.College.nonpriv

# Sort the Mahalanobis distances

sort.mah.College.nonpriv <- sort(mah.College.nonpriv,index.return=TRUE)$x
sort.mah.College.nonpriv

# Plot the sorted distances

plot(sort.mah.College.nonpriv,pch=19,col="deepskyblue",xlab="",ylab="",main="Mahalanobis distances for the nonprivate Colleges")

##################################################################################################################
# Use robust estimates of mean vector and covariance matrix
##################################################################################################################

install.packages("robustbase")
require("robustbase")

##################################################################################################################
# Private colleges

mcd.College.priv <- covMcd(College.priv)

rob.mv.College.priv <- mcd.College.priv$center
rob.mv.College.priv

rob.CM.College.priv <- mcd.College.priv$cov
rob.CM.College.priv

# Compute robust Mahalanobis distances for the private colleges

rob.mah.College.priv <- mahalanobis(College.priv,rob.mv.College.priv,rob.CM.College.priv)
rob.mah.College.priv

# Sort the robust Mahalanobis distances

sort.rob.mah.College.priv <- sort(rob.mah.College.priv,index.return=TRUE)$x
sort.rob.mah.College.priv

# Plot the sorted robust distances

plot(sort.rob.mah.College.priv,pch=19,col="deepskyblue",xlab="",ylab="",main="Robust Mahalanobis distances for the private Colleges")

##################################################################################################################
# NonPrivate colleges

mcd.College.nonpriv <- covMcd(College.nonpriv)

rob.mv.College.nonpriv <- mcd.College.nonpriv$center
rob.mv.College.nonpriv

rob.CM.College.nonpriv <- mcd.College.nonpriv$cov
rob.CM.College.nonpriv

# Compute robust Mahalanobis distances for the nonprivate colleges

rob.mah.College.nonpriv <- mahalanobis(College.nonpriv,rob.mv.College.nonpriv,rob.CM.College.nonpriv)
rob.mah.College.nonpriv

# Sort the robust Mahalanobis distances

sort.rob.mah.College.nonpriv <- sort(rob.mah.College.nonpriv,index.return=TRUE)$x
sort.rob.mah.College.nonpriv

# Plot the sorted robust distances

plot(sort.rob.mah.College.nonpriv,pch=19,col="deepskyblue",xlab="",ylab="",main="Robust Mahalanobis distances for the nonprivate Colleges")

##################################################################################################################
# Filling missing values
##################################################################################################################

install.packages("mice")
library(mice)

# Remember the births2006.smpl data set

?births2006.smpl

summary(births2006.smpl)

# There are missing values in TBO_REC, WTGAIN, APGAR5, and DBWT
# Moreover, in DMETH_REC appears the class Unknown that can be seen also as missing values

# Redefine the class Unknown as NA for the observations of the variable DMETH_REC

births2006 <- births2006.smpl
head(births2006)

# Replace the levels

summary(births2006$DMETH_REC)
levels(births2006$DMETH_REC)

levels(births2006$DMETH_REC) <- c("C-section",NA,"Vaginal")
summary(births2006$DMETH_REC)

# For saving computational time, consider only the first 1000 rows

births2006.1000 <- births2006[1:1000,]
head(births2006.1000)
summary(births2006.1000)

# Impute the missing values with unconditional mean imputation

imp.births2006.1000 <- mice(births2006.1000,m=1,method="mean")

# See the completed data set

imp.births2006.1000.data <- complete(imp.births2006.1000)
head(imp.births2006.1000.data)
summary(imp.births2006.1000.data)

# Note that for discrete variables it is required to round the imputed values

##################################################################################################################
# Sparse covariance matrix estimation for Colleges dataset
##################################################################################################################

# Install and load the library spcov if it is not already installed

install.packages("spcov")
library(spcov)

# Define the matrix P as the matrix of 1's of dimension p but with 0's in the main diagonal

P <- matrix(1,p.spam,p.spam)
diag(P) <- 0

# lam is the penalty parameter and step.size is the step size to use in generalized gradient descent
# See what happens if we take a small value of lam

lam <- 0.01
step.size <- 100

# Sigma is an initial guess for the covariance matrix and S is the sample covariance matrix
# Fix both as the sample covariance matrix

Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam*P,step.size=step.size)
summary(Sparse.S.spam.X)

Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
Sparse.Cov.spam.X

# See that the difference between both matrices is very small

sqrt(mean((Sparse.Cov.spam.X - S.spam.X)^2))

# Consider several values of lam

lam <- seq(0.1,1,by=0.1)
lam
dif.Covs <- matrix(NA,nrow=10,ncol=1)
for (i in 1:10){
  Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam[i]*P,step.size=step.size)
  Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
  dif.Covs[i] <- sqrt(mean((Sparse.Cov.spam.X - S.spam.X)^2))
}    
plot(1:10,dif.Covs,pch=19,main="Differences between covariances",col="deepskyblue")

# Take lam=0.4 for simplicity, although this can be refined

lam <- 0.4
Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam*P,step.size=step.size)
Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
Sparse.Cov.spam.X

# Sparse correlation matrix

Sparse.Cor.spam.X <- cov2cor(Sparse.Cov.spam.X)
Sparse.Cor.spam.X
