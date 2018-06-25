# -----------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistical Learning/Project")

# -----------------------------------------------------------------------------------------------
# Split the data set in two parts
class_movies <- movies.no.na.quant

# Define the data matrix and the response vector

# Regressor matrix
X = class_movies[, -12]
head(X)

# Response vector
y = class_movies[, 12]
head(y)

# Obtain sample size and dimensions of the data matrix
n.movies.X <- nrow(X)
n.movies.X
p.movies.X <- ncol(X)
p.movies.X

# Create the classes from the response vector
y[y >= 7] <- "good" 
y[y < 7 & y > 6] <- "average" 
y[y <= 6] <- "bad"

# Make y a factor variable
y <- as.factor(y)

# Compute number of movies in each group
n.good <- sum(y=="good")
n.good
n.average <- sum(y=="average")
n.average
n.bad <- sum(y=="bad")
n.bad

# Proportions of clients in each class
n.good / n.movies.X
n.average / n.movies.X
n.bad / n.movies.X
# -----------------------------------------------------------------------------------------------
# Make a parallel coordinates plot of the data matrix

# Average, bad, good
colors.y <- c("deepskyblue4","cyan","navy")[y]
head(colors.y)
head(y)

pairs(X,main='Scatter plot matrix',col=colors.y,pch=20)

# -----------------------------------------------------------------------------------------------
# Construct test and training sets

# Standardise the data
stan.X <- scale(X)
dim(stan.X)

# Construct training and testing sets
set.seed(2)
train <- sample(dim(class_movies)[1],dim(class_movies)[1]*0.7);train
length(train)
head(train)

# Obtain the training data set
X.train <- X[train,]
head(X.train)
y.train <- y[train]
head(y.train)

# Obtain testing set
X.test <- X[-train,]
head(X.test)
y.test <- y[-train]
head(y.test)

# length of train and testing set
n.train <- nrow(X.train)
n.test <- nrow(X.test)

# Check the proportions of the categories in both data sets
sum(y.train=="good")/n.train
sum(y.train=="average")/n.train
sum(y.train=="bad")/n.train

sum(y.test=="good")/n.test
sum(y.test=="average")/n.test
sum(y.test=="bad")/n.test

# -----------------------------------------------------------------------------------------------
# KNN 
# -----------------------------------------------------------------------------------------------
# Standardise the data
stan.X.train <- scale(X.train)
stan.X.test <- scale(X.test)

# Make it a dataframe
stan.X.train <- data.frame(stan.X.train)
stan.X.test <- data.frame(stan.X.test)

# We take K ranging from 1 to 50 
test.error <- matrix(NA,nrow=50,ncol=1)
for (i in 1 : 50){
  print(i)
  knn.out <- knngow(cbind(stan.X.train,y.train),stan.X.test,k=i)
  test.error[i] <- 1 - mean(knn.out==y.test)
}
plot(1:50,test.error,pch=20,col="deepskyblue4",type="b", main="", xlab="Number of trees", ylab="Test error")

# Select K with minimum test error
K <- which.min(test.error)
K

# Have a look at the solution with the chosen K. 
knn.out.K <- knngow(cbind(stan.X.train,y.train),stan.X.test,k=K)
head(knn.out.K)
knn.out.K

# Number of movies classified in each group
table(knn.out.K)

# Table with good and bad classifications
table(y.test,knn.out.K)

# Obtain the test error
test.error <- 1 - mean(knn.out.K==y.test)
test.error

# -----------------------------------------------------------------------------------------------
# Logistic regression
# -----------------------------------------------------------------------------------------------

# Fit the model with the training samples
lr.train.model <- multinom(y.train ~ .,data=X.train)
options(digit=4)

# Estimated coefficients and standard errors
summary(lr.train.model)

# Calculate the Z value (T-test)
t.train <- summary(lr.train.model)$coefficients/summary(lr.train.model)$standard.errors
t.train

# Calculate the p-value
p.value <- (1 - pnorm(abs(t.train), 0, 1))*2
exp(p.value)

# Extract coefficients from the train model
exp(coef(lr.train.model))

# Predict the class of the test sample
lr.test.pred <- predict(lr.train.model,newdata=X.test)
head(lr.test.pred)

# Number of movies classified in each group
table(lr.test.pred)

# Table with good and bad classifications
table(y.test,lr.test.pred)

# The test error
lr.error <- 1 - mean(lr.test.pred==y.test)
lr.error

# Print variance importance
varImp(lr.train.model)

# Obtain the probabilities
lr.test.probs <- predict(lr.train.model,newdata=X.test,"probs")
lr.test.probs <- data.frame(lr.test.probs)
head(lr.test.probs)

# Mean probabilities
mean(lr.test.probs$average)
mean(lr.test.probs$good)
mean(lr.test.probs$bad)

# Make a plot of the errors made and the probabilities
colors.errors <- c("cyan","deepskyblue4")[1*(lr.test.pred==y.test)+1]
par(mfrow=c(1,3))
plot(1:n.test,lr.test.probs$bad,main='Classification of bad movies',col=colors.errors,pch=20,type="p",xlab="Test sample",ylab="Probabilities")
plot(1:n.test,lr.test.probs$good,main='Classification of good movies',col=colors.errors,pch=20,type="p",xlab="Test sample",ylab="Probabilities")
plot(1:n.test,lr.test.probs$average,main='Classification of average movies',col=colors.errors,pch=20,type="p",xlab="Test sample",ylab="Probabilities")


  

