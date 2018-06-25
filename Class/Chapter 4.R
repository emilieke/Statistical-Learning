
##################################################################################################################
##################################################################################################################
# Chapter 4 - Functional data analysis
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
# The Canadian weather data set
##################################################################################################################
##################################################################################################################

# Install and load the library fda, if it is not already installed

install.packages("fda")
library("fda")

# See the help page

?CanadianWeather

# Load the data set into memory

data(CanadianWeather)

# Check the type of object

is(CanadianWeather)

# Essentially, it is a list. How many objects?

summary(CanadianWeather)

# Note that there is a lot of information regarding the data set including the place and province of the 35 weather
# stations

##################################################################################################################
# Focus on dailyAv

# Check the type of the object called dailyAv

is(CanadianWeather$dailyAv)

# This is an array. Which dimension?

dim(CanadianWeather$dailyAv)

# This means that there are three matrices of sizes 365x35

# The first matrix corresponds to the average daily temperature for each day of the year at the 35 stations

head(CanadianWeather$dailyAv[,,1])

# The second matrix corresponds to the average daily rainfall for each day of the year rounded to 0.1 mm. at the 35 stations

head(CanadianWeather$dailyAv[,,2])

# The third matrix corresponds to the base 10 logarithm of Precipitation.mm after first replacing 27 zeros by 0.05 mm at the 35 stations

head(CanadianWeather$dailyAv[,,3])

##################################################################################################################
# Data treatment for the average daily temperature for each day of the year

# Define a matrix in which the rows contains the data corresponding to the average daily temperature for each day 
# of the year each for each weather station. Therefore, we take the transpose of the first matrix in dailyAv

X <- t(CanadianWeather$dailyAv[,,1])
head(X)

# Size of the matrix

n <- dim(X)[1]
n
p <- dim(X)[2]
p

# Define a vector with the points at which the data is defined

tp <- 1 : p
tp

# Install and load the library fda.usc, if it is not already installed

install.packages("fda.usc")
library("fda.usc")

# Create an fdata object from X

X.fda <- fdata(X,tp)

##################################################################################################################
# Plot the data

plot.fdata(X.fda,type="l",col="deepskyblue4",main="Weather stations data set",xlab="Day",ylab="Average daily temperature ºC")

##################################################################################################################
##################################################################################################################
# The phoneme data set
##################################################################################################################
##################################################################################################################

# See the help page

?phoneme

# Load the data set into memory

data(phoneme)

# Check the type of object

is(phoneme)

# Essentially, it is a list. How many objects?

summary(phoneme)

# Note that phoneme is split into a training set, called learn, and a test set, called test

phoneme.X.training <- phoneme[["learn"]]
phoneme.Y.training <- phoneme[["classlearn"]]

phoneme.X.test <- phoneme[["test"]]
phoneme.Y.test <- phoneme[["classtest"]]

##################################################################################################################
# Plot the data

colors.phoneme.training <- c("deepskyblue4","firebrick4","orange","chartreuse","darkviolet")[phoneme.Y.training]
plot.fdata(phoneme.X.training,type="l",col=colors.phoneme.training,main="Phonemes data set",xlab="Frequency",ylab="Log-periodogram")

##################################################################################################################
##################################################################################################################
# Functional principal component analysis for the Canadian weather data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Plot the data

plot.fdata(X.fda,type="l",col="deepskyblue4",main="Weather stations data set",xlab="Day",ylab="Average daily temperature ºC")

# Obtain the PCs

PCs.X.fda <- create.pc.basis(X.fda)

# Have a look at the objects in basis.X.fda

names(PCs.X.fda)

# The object "mean" is the sample mean function that we can plot in the previous figure

lines(PCs.X.fda$mean,col="firebrick4",lwd=4)

# The object "basis" constains the PCs. Particularly, "basis$data" contains the numeric matrix

PCs.X.fda$basis$data

# We plot the first three PCs

par(mfrow=c(3,1))
plot(PCs.X.fda$basis[1],col="deepskyblue4",main="First PC for the weather stations data set",xlab="Day",ylab="Average daily temperature ºC")
plot(PCs.X.fda$basis[2],col="deepskyblue4",main="Second PC for the weather stations data set",xlab="Day",ylab="Average daily temperature ºC")
plot(PCs.X.fda$basis[3],col="deepskyblue4",main="Third PC for the weather stations data set",xlab="Day",ylab="Average daily temperature ºC")

# See that:
# 1. The first eigenfunction says that the largest variability appears in winter while in summer the variability is much smaller
# 2. The second eigenfunction contrasts the variability of the summer against the variability of the winter
# 3. The third eigenfunction Contrasts the variability of the first part of the year against the variability of the last part of the year

# The scores are located in the object "x"

head(PCs.X.fda$x)

# Note that the scores for the PCs starting at 36 are equal to 0 (as they should be)

# Obtain the eigenvalues

PCs.X.fda.values <- apply(PCs.X.fda$x,2,var)[1:35]
PCs.X.fda.values

# Plot the eigenvalues

par(mfrow=c(1,1))
plot(1:n,PCs.X.fda.values,xlab="Number",ylab="Eigenvalues",main="Screeplot",col="deepskyblue4",pch=20,type="p")

# Obtain the proportion of accumulated variance

prop.acum.car <- cumsum(PCs.X.fda.values / sum(PCs.X.fda.values))
prop.acum.car

# The first two PCs explain the 96.44% of the total variability of the data set

# Plot the first two PCs scores

plot(PCs.X.fda$x[,1],PCs.X.fda$x[,2],pch=20,col="deepskyblue4",xlab="First PC",ylab="Second PC")
text(PCs.X.fda$x[,1:2],labels=CanadianWeather$place,pos = 1,col="firebrick4",cex=0.5)

# These PCs scores give us locations with similar behavior in terms of variability

##################################################################################################################
##################################################################################################################
# Unsupervised classification for functional data with the K-means method for the Canadian weather data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Apply the K-means algorithm for K=2 to the Canadian weather data set

kmeans.X.fda <- kmeans.fd(X.fda,ncl=2,iter=100,draw=FALSE)
names(kmeans.X.fda)

# See the assigment made by the algorithm

kmeans.X.fda$cluster

# How many functions in each cluster

table(kmeans.X.fda$cluster)

# Plot the curves

par(mfrow=c(1,1))
colors.kmeans.X.fda <- c("deepskyblue4","firebrick4")[kmeans.X.fda$cluster]
plot.fdata(X.fda,type="l",col=colors.kmeans.X.fda,main="Weather stations data set",xlab="Day",ylab="Average daily temperature ºC")

# Plot also the first two PCs scores

plot(PCs.X.fda$x[,1],PCs.X.fda$x[,2],pch=20,col=colors.kmeans.X.fda,xlab="First PC",ylab="Second PC")

##################################################################################################################
# Apply the K-means algorithm for K=3 to the Canadian weather data set

kmeans.X.fda <- kmeans.fd(X.fda,ncl=3,iter=100,draw=FALSE)
names(kmeans.X.fda)

# See the assigment made by the algorithm

kmeans.X.fda$cluster

# How many functions in each cluster

table(kmeans.X.fda$cluster)

# Plot the curves

par(mfrow=c(1,1))
colors.kmeans.X.fda <- c("deepskyblue4","firebrick4","orange")[kmeans.X.fda$cluster]
plot.fdata(X.fda,type="l",col=colors.kmeans.X.fda,main="Weather stations data set",xlab="Day",ylab="Average daily temperature ºC")

# Plot also the first two PCs scores

plot(PCs.X.fda$x[,1],PCs.X.fda$x[,2],pch=20,col=colors.kmeans.X.fda,xlab="First PC",ylab="Second PC")

##################################################################################################################
# Apply the K-means algorithm for K=4 to the Canadian weather data set

kmeans.X.fda <- kmeans.fd(X.fda,ncl=4,iter=100,draw=FALSE)
names(kmeans.X.fda)

# See the assigment made by the algorithm

kmeans.X.fda$cluster

# How many functions in each cluster

table(kmeans.X.fda$cluster)

# Plot the curves

par(mfrow=c(1,1))
colors.kmeans.X.fda <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmeans.X.fda$cluster]
plot.fdata(X.fda,type="l",col=colors.kmeans.X.fda,main="Weather stations data set",xlab="Day",ylab="Average daily temperature ºC")

# Plot also the first two PCs scores

plot(PCs.X.fda$x[,1],PCs.X.fda$x[,2],pch=20,col=colors.kmeans.X.fda,xlab="First PC",ylab="Second PC")

##################################################################################################################
##################################################################################################################
# Supervised classification for functional data with the K-means method for the Canadian weather data set
##################################################################################################################
##################################################################################################################

# The function classif.knn performs KNN classification with cross-validation in the training sample to select
# the number of neighbors, while the error rate is estimated with the test sample

phoneme.knn <- classif.knn(phoneme.Y.training,phoneme.X.training,knn=1:10)
summary(phoneme.knn)

# Then, the selected number of neighbors are used to classify the test sample

phoneme.knn.test <- predict(phoneme.knn,phoneme.X.test,type="probs")
summary(phoneme.knn.test)

# See the assigment made by the algorithm

phoneme.knn.test$group.pred

# Number of phonemes classified in each group

table(phoneme.knn.test$group.pred)

# Table with good and bad classifications

table(phoneme.Y.test,phoneme.knn.test$group.pred)

# The test error is

test.error.phoneme <- 1 - mean(phoneme.knn.test$group.pred==phoneme.Y.test)
test.error.phoneme

# The probabilities of each group are

head(phoneme.knn.test$prob.group)

# The maximum probabilities are

apply(phoneme.knn.test$prob.group,1,max)

# As there is only one neighbor, all the estimated probabilities are equal to 1
