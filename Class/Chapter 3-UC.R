
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
# Centroid-based clustering analysis for the NCI60 data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Install and load ISLR package

install.packages("ISLR")
library("ISLR")

# See the help page

?NCI60

# Load the data set into memory

data(NCI60)

# Remember that NCI60 is a list with two objects

summary(NCI60)

# The data matrix is the object called data

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
# Remember: PCA for the NCI60 data set

PCS.NCI60 <- prcomp(X)

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.NCI60$x[,1:2],pch=20,col="deepskyblue4")

# This plot suggests that there migth be some heterogeneity, i.e., at least two clusters

# Remember that around 17 (or 21) PCs where needed to explain almost the 70% of the total 
# variability of the dataset. Thus, the number of clusters might be more than 2

##################################################################################################################
# Perform k-means clustering for the NCI60 data set
##################################################################################################################

# Try K=2 and see what happens
# We take 100 initial random solutions

kmeans.X <- kmeans(X,centers=2,iter.max=1000,nstart=100)

# The function returns a large list of objects. 

summary(kmeans.X)

# The final partition is given in cluster

kmeans.X$cluster

# The number of observations in each cluster

kmeans.X$size

# The sample mean vectors of the clusters are in centers
# kmeans.X$centers
# Not run in this case

# The total sum of squares

kmeans.X$totss

# Vector of within-cluster sum of squares

kmeans.X$withinss 

# Total within-cluster sum of squares

kmeans.X$tot.withinss 

# The between-cluster sum of squares, i.e. totss-tot.withinss

kmeans.X$betweenss 

# Check that the two numbers is the same

kmeans.X$tot.withinss+kmeans.X$betweenss
kmeans.X$totss

##################################################################################################################
# Compare several values of K and select the most appropriate one

# We run K-means for K ranging from K=2 to K=10

WSS <- matrix(NA,nrow=9,ncol=1)
BSS <- matrix(NA,nrow=9,ncol=1)
Ratio.WSS.BSS <- matrix(NA,nrow=9,ncol=1)
for (k in 1:9){
  print(k)
  kmeans.X <- kmeans(X,centers=k+1,iter.max=1000,nstart=100)
  WSS[k] <- kmeans.X$tot.withinss
  BSS[k] <- kmeans.X$betweenss
  Ratio.WSS.BSS[k] <- WSS[k]/BSS[k]
}
plot(2:10,Ratio.WSS.BSS,col="deepskyblue4",pch=20,main="Ratio for different values of K",xlab="K",ylab="Ratio")
segments(x0=2:10,y0=Ratio.WSS.BSS,y1=0,col="deepskyblue4")

# The plot appears to suggests the presence of 4 groups, although 3 or 5 groups can be also reasonable

# Take 4 clusters

kmeans.X <- kmeans(X,centers=4,iter.max=1000,nstart=100)

# Make a plot of the first two PCs split in these four clusters

colors.kmeans.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmeans.X$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmeans.X)

##################################################################################################################
# K-means with PCs

# We take 21 PCs as in Chapter 2

kmeans.PCS.X <- kmeans(PCS.NCI60$x[,1:21],4,iter.max=1000,nstart=100)

# Make a plot of the first two PCs split in these four clusters

colors.kmeans.PCS.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmeans.PCS.X$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmeans.PCS.X)

cbind(kmeans.X$cluster,kmeans.PCS.X$cluster)

# The solutions are very close. Which is better? Have a look at the silhouette plot

# Silhouette plot for the original data set

install.packages("cluster")
library(cluster)
diss.X <- daisy(X)
sil.X <- silhouette(kmeans.X$cluster,diss.X^2)
plot(sil.X)

# Note that the we take the square of the distances because kmeans uses
# the squared Euclidean distance

# Silhouette plot for the original data set using the cluster solution using PCs

sil.PCS.X <- silhouette(kmeans.PCS.X$cluster,diss.X^2)
plot(sil.PCS.X)

# Thus, the two solutions appears to be similar
# However note that the original data set uses 6830 variables
# and the PCs only uses 21 variables!!!

##################################################################################################################
# Perform k-medians clustering for the NCI60 data set
##################################################################################################################

install.packages("flexclust")
library("flexclust")

kmedians.X <- kcca(X,k=4,family=kccaFamily("kmedians"))

# Make a plot of the first two PCs split in these four clusters

colors.kmedians.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[clusters(kmedians.X)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmedians.X)

# Have a look at the silhouette

diss.X <- daisy(X,metric="manhattan")
sil.X <- silhouette(clusters(kmedians.X),diss.X)
plot(sil.X)

# Apparently, this is a bad solution for this data set.

##################################################################################################################
# Perform k-medoids clustering for the NCI60 data set
##################################################################################################################

# Take 4 clusters

kmedoids.X <- pam(X,k=4,metric="euclidean",stand=FALSE)

# Make a plot of the first two PCs split in these four clusters

colors.kmedoids.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmedoids.X$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmedoids.X)

# Have a look at the silhouette

diss.X <- daisy(X,metric="euclidean")
sil.X <- silhouette(kmedoids.X$cluster,diss.X)
plot(sil.X)

# Apparently, this is also a bad solution for this data set.

##################################################################################################################
# Perform CLARA clustering for the NCI60 data set
##################################################################################################################

# Take 4 clusters

clara.X <- clara(X,k=4,metric="euclidean",stand=FALSE,samples=100,sampsize=32)

# Make a plot of the first two PCs split in these four clusters

colors.clara.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[clara.X$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.clara.X)

# Have a look at the silhouette

diss.X <- daisy(X,metric="euclidean")
sil.X <- silhouette(clara.X$cluster,diss.X)
plot(sil.X)

# The solution is similar to the previous one because the sample size here is small.

##################################################################################################################
##################################################################################################################
# Hierarchical clustering analysis for the NCI60 data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# First, we compute the distance matrix between the observations in the sample.
# For that, we make use of the function dist that allows to compute the Euclidean and the Manhattan distance,
# among others.

##################################################################################################################
# Use the Euclidean distance

distances <- dist(X,method="euclidean",upper=TRUE)
distances

##################################################################################################################
# Single linkage

single.X <- hclust(distances,method="single")

# The assigment into 4 clusters 

cutree(single.X,4)

# Plot the dendogram

plot(single.X,main="Euclidean distance and single linkage",cex=0.8)
rect.hclust(single.X,k=4,border="deepskyblue4")

# Another alternatives

install.packages("ape")
library(ape)

plot(as.phylo(single.X),cex=0.8,label.offset=1)
plot(as.phylo(single.X),type="fan")

# Make a plot of the first two PCs split in these four clusters

colors.single.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(single.X,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.single.X)

# Have a look at the silhouette

diss.X <- daisy(X,metric="euclidean")
sil.X <- silhouette(cutree(single.X,4),diss.X)
plot(sil.X)

# This solution is awful

##################################################################################################################
# Complete linkage

complete.X <- hclust(distances,method="complete")

# The assigment into 4 clusters 

cutree(complete.X,4)

# Plot the dendogram

plot(complete.X,main="Euclidean distance and complete linkage",cex=0.8)
rect.hclust(complete.X,k=4,border="deepskyblue4")

# Another alternatives

plot(as.phylo(complete.X),cex=0.8,label.offset=1)
plot(as.phylo(complete.X),type="fan")

# Make a plot of the first two PCs split in these four clusters

colors.complete.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(complete.X,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.complete.X)

# Have a look at the silhouette

sil.X <- silhouette(cutree(complete.X,4),diss.X)
plot(sil.X)

# This solution is not good either

##################################################################################################################
# Average linkage

average.X <- hclust(distances,method="average")

# The assigment into 4 clusters 

cutree(average.X,4)

# Plot the dendogram

plot(average.X,main="Euclidean distance and average linkage",cex=0.8)
rect.hclust(average.X,k=4,border="deepskyblue4")

# Another alternatives

plot(as.phylo(average.X),cex=0.8,label.offset=1)
plot(as.phylo(average.X),type="fan")

# Make a plot of the first two PCs split in these four clusters

colors.average.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(average.X,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.average.X)

# Have a look at the silhouette

sil.X <- silhouette(cutree(average.X,4),diss.X)
plot(sil.X)

# This solution is not good either

##################################################################################################################
# Ward method

ward.D2.X <- hclust(distances,method="ward.D2")

# The assigment into 4 clusters 

cutree(ward.D2.X,4)

# Plot the dendogram

plot(ward.D2.X,main="Euclidean distance and ward linkage",cex=0.8)
rect.hclust(ward.D2.X,k=4,border="deepskyblue4")

# Another alternatives

plot(as.phylo(ward.D2.X),cex=0.8,label.offset=1)
plot(as.phylo(ward.D2.X),type="fan")

# Make a plot of the first two PCs split in these four clusters

colors.ward.D2.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(ward.D2.X,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.ward.D2.X)

# Have a look at the silhouette

sil.X <- silhouette(cutree(ward.D2.X,4),diss.X)
plot(sil.X)

# This solution is slightly better

##################################################################################################################
##################################################################################################################
# Hierarchical clustering analysis for the flower data set
##################################################################################################################
##################################################################################################################

# Load data set named flower

data(flower)
?flower

X.flower <- flower

##################################################################################################################
# Ward method

# Obtain the Gower distances

distances <- daisy(X.flower,metric="gower")

# Perform hierarchical clustering with Ward linkage

ward.D2.X.flower <- hclust(distances,method="ward.D2")

# Plot the dendogram

plot(ward.D2.X.flower,main="Euclidean distance and ward linkage",cex=0.8)
rect.hclust(ward.D2.X.flower,k=3,border="deepskyblue4")

# Another alternatives

plot(as.phylo(ward.D2.X.flower),cex=0.8,label.offset=1)
plot(as.phylo(ward.D2.X.flower),type="fan")

# Have a look at the silhouette

sil.X <- silhouette(cutree(ward.D2.X.flower,3),distances)
plot(sil.X)

# The solution is quite good

##################################################################################################################
##################################################################################################################
# Model-based clustering for the NCI60 data set
##################################################################################################################
##################################################################################################################

install.packages("mclust")
library(mclust)

##################################################################################################################
# Remember: PCA for the NCI60 data set

PCS.NCI60 <- prcomp(X)
summary(PCS.NCI60)

# Consider 17 PCs to perform model-based clustering

X.Mclust <- Mclust(PCS.NCI60$x[,1:17],G=1:5)
summary(X.Mclust)

# Have a look at the solution in the first two PCs

colors.Mclust.X <- c("deepskyblue4","firebrick4","orange","chartreuse","darkviolet")[X.Mclust$classification]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.Mclust.X)

# See the 17 PCS

pairs(PCS.NCI60$x[,1:17],col=colors.Mclust.X,main="M-clust solution",pch=19)

# Is it reasonable?
