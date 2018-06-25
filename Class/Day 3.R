
##################################################################################################################
##################################################################################################################
# Chapter 1 - Multidimensional data - Third day
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
# Control of the number of decimals in the outputs

options(digits=4)

##################################################################################################################
# Examples of well structured data sets
##################################################################################################################

##################################################################################################################
# The Spam data set

# Install and load the library kernlab

install.packages("kernlab")
library("kernlab")

data(spam)

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

##################################################################################################################
# The NCI60 data set

# Install and load the library ISLR

install.packages("ISLR")
library("ISLR")

data(NCI60)

# Sample sizes and dimensions of the NCI60 data set

n.NCI60 <- dim(NCI60$data)[1]
n.NCI60
p.NCI60 <- dim(NCI60$data)[2]
p.NCI60

##################################################################################################################
# The births2006 data set

# Install and load the library nutshell

install.packages("nutshell")
library("nutshell")

data(births2006.smpl)

# Sample sizes and dimensions of the births2006.smpl data set

n.NCI60 <- dim(births2006.smpl)[1]
n.NCI60
p.NCI60 <- dim(births2006.smpl)[2]
p.NCI60

##################################################################################################################
# The College data set

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

##################################################################################################################
# Mahalanobis distances for the College data set
##################################################################################################################

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

S.spam.X <- cov(spam.X)

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

##################################################################################################################
##################################################################################################################
# Chapter 2 - Dimension reduction techniques
##################################################################################################################
##################################################################################################################

##################################################################################################################
# PCA for the NCI60 data set
##################################################################################################################

# Define the data matrix

X <- NCI60$data

# Size of the matrix

dim(X)

##################################################################################################################
# Sample sizes and dimensions of the NCI60 data set

n.NCI60 <- dim(X)[1]
n.NCI60
p.NCI60 <- dim(X)[2]
p.NCI60

##################################################################################################################
# There are many different functions in R to perform PCA 
# Next, we use the function prcomp
# All the variables in the NCI60 data set have the same units of measurement. Therefore, we use the sample 
# covariance matrix

PCS.NCI60 <- prcomp(X)

# Have a look at the outputs

names(PCS.NCI60)

##################################################################################################################
# The eigenvectors of the sample covariance matrix are given in rotation
# The output is a matrix of size 6830x64, so we only have a look at the first few rows

dim(PCS.NCI60$rotation)
head(PCS.NCI60$rotation)

# Note that only 64 eigenvectors (corresponding to 64 PCs) appear
# This is because Sx has n<p and then only 64 eigenvalues can be different than 0

##################################################################################################################
# The PC scores is are in x. In this case, the scores are a matrix of size 64x64

dim(PCS.NCI60$x)
PCS.NCI60$x

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.NCI60$x[,1:2],pch=20,col="deepskyblue4")

# Remember that we are interested in make groups of different cancer types
# The first two PCs suggest the presence of at least two different groups
# This information is completely hidden if we consider the 6830 variables

# We can add the names of the cancer

text(PCS.NCI60$x[,1:2],labels=NCI60$labs,pos = 1,col="firebrick4",cex=0.5)

##################################################################################################################
# Interpretation of the first PC: The first eigenvector has 6830 values
# Each value represent the weight of the associated variable (the measurement on a particular gen)
# Have a look at the important variables (genes) in the first PC

plot(PCS.NCI60$rotation[,1],pch=20,col="deepskyblue4",main="Weights for the first PC")
abline(h=0)

# Have a look at the important variables (genes) in the second PC

plot(PCS.NCI60$rotation[,2],pch=20,col="deepskyblue4",main="Weights for the second PC")
abline(h=0)

# Have a look at the important variables (genes) in the first two PCs

plot(PCS.NCI60$rotation[,1:2],pch=20,col="deepskyblue4",main="Weights for the first two PCs")
abline(h=0,v=0)
text(PCS.NCI60$rotation[,1:2],labels=colnames(NCI60$data),pos = 1,col="firebrick4",cex=0.5)

# Those genes that appear to be outliers are those genes that creates the largest variability in
# the first two PCs, which are the most important ones

##################################################################################################################
# Make a plot of the first three PCs

install.packages("scatterplot3d")
library("scatterplot3d")

# Three-variate scatterplot

s3d.NCI60 <- scatterplot3d(PCS.NCI60$x[,1:3],pch=20,color="deepskyblue4",main="3D Scatterplot",type="h")
text(s3d.NCI60$xyz.convert(PCS.NCI60$x[,1:3]),labels=NCI60$labs,pos = 1,col="firebrick4",cex=0.5)

# Three-dimensional scatterplot of the Weights

scatterplot3d(PCS.NCI60$rotation[,1:3],pch=20,color="deepskyblue4",main="3D Scatterplot")

##################################################################################################################
# How many PCs?

# Screeplot with the 64 eigenvalues

screeplot(PCS.NCI60,npcs=64,main="Screeplot",col="deepskyblue4",type="lines",pch=20)

# Have a look at the proportion of explained variance and the cumulative proportion of explained variance

summary(PCS.NCI60)

# We need 21 PCs to have the 70% of the total variability of the data set.
# The mean of the eigenvalues can be obtained as follows

EVAL.NCI60 <- PCS.NCI60$sdev^2
mean(EVAL.NCI60)

# The number of eigenvalues larger than the mean of them is 

sum(EVAL.NCI60>mean(EVAL.NCI60))

# In any case, we reduce the dimension of the data set from 6830 to 21 or 17. That is we consider
# either the 0.30% or the 0.24% of the number of variables in the data set keeping around the 70%
# of the information inside

##################################################################################################################
# PCA for the College data set
##################################################################################################################

##################################################################################################################
# We define a new matrix with the quantitative variables and one vector with the qualitative variable (Private)

X <- College[,2:18]
head(X)

Y <- College[,1]
head(Y)
table(Y)

##################################################################################################################
# Define sample size and the dimension

dim(X)
n.X <- dim(X)[1]
n.X
p.X <- dim(X)[2]
p.X

##################################################################################################################
# Plot the original variables

library(MASS)

colors.X <- c("deepskyblue4","firebrick4")[Y]
parcoord(X,col=colors.X,var.label=TRUE)

##################################################################################################################
# PCA on the whole data set
# Use the sample correlation matrix

PCS.X <- prcomp(X,scale = TRUE)

##################################################################################################################
# Eigenvectors of the sample correlation matrix

dim(PCS.X$rotation)
PCS.X$rotation

##################################################################################################################
# PC scores

dim(PCS.X$x)
head(PCS.X$x)

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.X$x[,1:2],pch=20,col=colors.X)

# The first two PCs show the presence of the two different groups
# This information was not clear in a plot of the 17 variables

##################################################################################################################
# Interpretation of the first PC: Weights for the first PC

plot(1:p.X,PCS.X$rotation[,1],pch=20,col="deepskyblue4",main="Weights for the first PC")
abline(h=0)
text(1:p.X,PCS.X$rotation[,1],labels=colnames(X),pos=1,col="firebrick4",cex=0.5)

##################################################################################################################
# Interpretation of the second PC: Weights for the second PC

plot(1:p.X,PCS.X$rotation[,2],pch=20,col="deepskyblue4",main="Weights for the second PC")
abline(h=0)
text(1:p.X,PCS.X$rotation[,2],labels=colnames(X),pos=1,col="firebrick4",cex=0.5)

##################################################################################################################
# Have a look at the important variables in the first two PCs
# Note the different groups in the data

plot(PCS.X$rotation[,1:2],pch=20,col="deepskyblue4",main="Weights for the first two PCs")
abline(h=0,v=0)
text(PCS.X$rotation[,1:2],labels=colnames(X),pos=1,col="firebrick4",cex=0.5)

##################################################################################################################
# How many PCs?

# Screeplot with the 17 eigenvalues

screeplot(PCS.X,npcs=17,main="Screeplot",col="deepskyblue4",type="lines",pch=20)

# Have a look at the proportion of explained variance and the cumulative proportion of explained variance

summary(PCS.X)

# We need 4 PCs to have the 70% of the total variability of the data set.
# As we are using the sample correlation matrix, the mean of the eigenvalues 
# is equal to 1. Check it

EVAL.X <- PCS.X$sdev^2
mean(EVAL.X)

# The number of eigenvalues larger than the mean of them is 

sum(EVAL.X>mean(EVAL.X))

# In this case, we reduce the dimension of the data set from 17 to 4. That is we consider
# either the 23.53% of the number of variables in the data set keeping around the 70%
# of the information inside

##################################################################################################################
# Plot the scores (the four PCs)

pairs(PCS.X$x[,1:4],col=colors.X,pch=20,main="The first four PCs")

# The PCs show that the variables have different behavior in terms of the two groups

##################################################################################################################
# Detect outliers with the PCs
##################################################################################################################

##################################################################################################################
# If we look for outliers in a data set like this, it is better to look in the two groups separately
# Here, we consider only the private colleges

##################################################################################################################
# Define the subset of private colleges

X.priv <- X[Y=="Yes",]
head(X.priv)

dim(X.priv)
n.priv <- dim(X.priv)[1]
n.priv
p.priv <- dim(X.priv)[2]
p.priv

##################################################################################################################
# Obtain PCs in the private colleges

PCS.X.priv <- prcomp(X.priv,scale = TRUE)

##################################################################################################################
# Select the number of PCs

screeplot(PCS.X.priv,npcs=17,main="Screeplot",col="deepskyblue4",type="lines",pch=20)

# Three appears to be ok

##################################################################################################################
# Install and load robustbase package

install.packages("robustbase")
require("robustbase")

##################################################################################################################
# Compute robust estimates of mean vector and covariance matrix

mcd.X.priv <- covMcd(PCS.X.priv$x[,1:3])

rob.mv.X.priv <- mcd.X.priv$center
rob.mv.X.priv

rob.CM.X.priv <- mcd.X.priv$cov
rob.CM.X.priv

# Compute robust Mahalanobis distances for the PCs of private colleges

rob.mah.X.priv <- mahalanobis(PCS.X.priv$x[,1:3],rob.mv.X.priv,rob.CM.X.priv)
rob.mah.X.priv

# Sort the robust Mahalanobis distances

sort.rob.mah.X.priv <- sort(rob.mah.X.priv,index.return=TRUE)$x
sort.rob.mah.X.priv

# Plot the sorted robust distances

plot(sort.rob.mah.X.priv,pch=19,col="deepskyblue",xlab="",ylab="",main="Robust Mahalanobis distances for the PCs of private Colleges")

##################################################################################################################
##################################################################################################################
# Sparse Principal components
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Install packages PMA 

install.packages("PMA")

##################################################################################################################
# Install package impute from this webpage:

source("https://bioconductor.org/biocLite.R")

# Installation of this library is done as follows: 
# When asked about to update some packages say n

biocLite("impute")

##################################################################################################################
# Load library PMA that loads library impute

library("PMA")

##################################################################################################################
# Define X.p as a matrix object

X.mat <- as.matrix(X)
head(X.mat)

# Then, obtain the scaled variables

Y <- scale(X.mat)
head(Y)

##################################################################################################################
# To perform sparse principal component analysis, we run the function SPC
# For that, we need to fix the number k in the L1 norm restriction of the weights. 
# The results will depend on this selection: 
#   1. If k is taken too large, then, the standard and the sparse PCs will be the same.
#   2. If k is taken too small, then, there will be many 0s in the weights

# This value should be tuned. In this case, we take k=2.5

# It is important to note that sometimes we can obtain errors using this function
# because the optimizer does not converge or find some problems for numerical reasons.
# In these cases, the option is to reduce the number of PCs used

SPC.s <- SPC(Y,sumabsv=2.5,K=p.X,orth=TRUE)

# Have a look at the results

# First, we see the proportion of variance explained by the SPCs.
# The formulas are different than those from the standard PCs because the SPC
# are not obtained from eigenvectors and eigenvalues (not entering into details)

prop.var.expl <- SPC.s$prop.var.explained
prop.var.expl

# As you can see, the variance explained by the first 4 SPCs is smaller than the corresponding 
# to the first 4 PCs obtained previously. Anyway, for comparison purposes, we take 4 SPCs

# The weights are given by

V <- SPC.s$v[,1:4]
V

# As it can be seen, most of the weights are equal to 0.
# Therefore, PCs are associated only to some of the variables favouring the interpretation.

# Obtain the scores for these 4 SPCs

Z.S <- Y %*% V
head(Z.S)
colnames(Z.S) <- c("SPC1","SPC2","SPC3","SPC4")

# Note that, altough we do impose orthogonality in the optimization problem, we do not obtain 
# orthogonal sparse PCs as in the case of the standard PCs

cor(Z.S)

# Plot the scores

pairs(Z.S,col=colors.X,pch=20,main="The first four sparse PCs")

# Compare the PC scores with the sparse PC scores

par(mfrow=c(2,2))

plot(PCS.X$x[,1],Z.S[,1],col=colors.X,pch=20,main="Comparison of first scores",xlab="Fist PC",ylab="First SPC")
plot(PCS.X$x[,2],Z.S[,2],col=colors.X,pch=20,main="Comparison of second scores",xlab="Second PC",ylab="Second SPC")
plot(PCS.X$x[,3],Z.S[,3],col=colors.X,pch=20,main="Comparison of third scores",xlab="Third PC",ylab="Third SPC")
plot(PCS.X$x[,4],Z.S[,4],col=colors.X,pch=20,main="Comparison of fourth scores",xlab="Fourth PC",ylab="Fourth SPC")

##################################################################################################################
##################################################################################################################
# Independent Components Analysis
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Install and load fastICA package

install.packages("fastICA")
require("fastICA")

##################################################################################################################
# Obtain the ICs with the College data set. We use 4 ICs as in PCA and standarize the data

FICA.X <- fastICA(X,n.comp=4,row.norm=TRUE)

# Have a look at the ICA scores obtained

pairs(FICA.X$S,col=colors.X,pch=20,main="Four ICA components")

# Note that the ICs scores have zero mean and identity covariance matrix

colMeans(FICA.X$S)
cov(FICA.X$S)

# Have a look at the weights 

FICA.X$K
