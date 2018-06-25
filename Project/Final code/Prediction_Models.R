# -----------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistical Learning/Project")

# -----------------------------------------------------------------------------------------------
# Predicting movie rating
# -----------------------------------------------------------------------------------------------

# Get the correct data set
pred_movies<-pred.movies
names(pred_movies)

# -----------------------------------------------------------------------------------------------

# Construct training and testing sets
set.seed(2)
train <- sample(dim(pred_movies)[1],dim(pred_movies)[1]*0.7);
movie_train <- pred_movies[train,]
movie_test <- pred_movies[-train,]

# -----------------------------------------------------------------------------------------------
# Linear regression
# -----------------------------------------------------------------------------------------------

# Fit full linear regression model
model.full = lm(imdb_score~.,data=movie_train)
summary(model.full)
xtable(summary(model.full))

# The MSE on the test data set
pred <- predict(model.full,movie_test)
mean((movie_test$imdb_score-pred)^2)

# Variable importance
lr.varimp <- data.frame(varImp(model.full));lr.varimp
lr.varimp$Vars<-row.names(lr.varimp);lr.varimp$Vars
lr.varimp <- lr.varimp[order(lr.varimp$Overall, decreasing=TRUE),];lr.varimp

# Plot variance impotance
par(mar=c(4,10,2,2))
barplot(lr.varimp$Overall,horiz=TRUE, col="deepskyblue4",xlim=c(0, 15),
        names.arg=lr.varimp$Vars, las=1)

# Plots
library(car);

# Assessing Outliers
outlierTest(lmfit) # Bonferonni p-value for most extreme obs
# Normality of Residuals
qqPlot(lmfit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(lmfit) # leverage plots 

# Influential Observations
# added variable plots
avPlots(lmfit)

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(pred_movies)-length(lmfit$coefficients)-2))
plot(lmfit, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(lmfit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# distribution of studentized residuals
library(MASS)
sresid <- studres(model.full)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(lmfit)

# plot studentized residuals vs. fitted values
spreadLevelPlot(lmfit)

# Evaluate Collinearit
# Variance inflation factors
vif(lmfit)

# Evaluate Nonlinearity
# component + residual plot
crPlots(lmfit)

# Ceres plots
# TODO: problem here
ceresPlots(lmfit)

# Test for Autocorrelated Errors
durbinWatsonTest(lmfit)

# Global test of model assumptions
gvmodel <- gvlma(lmfit) 
summary(gvmodel)

AIC(lmfit);
BIC(lmfit);

# -----------------------------------------------------------------------------------------------
# Stepwise regression
# -----------------------------------------------------------------------------------------------

# Minimal model, only including intercept
model.empty = lm(imdb_score ~ 1, data = movie_train)

# Full model, including all variables
model.full = lm(imdb_score ~ ., data = movie_train)

# Scope of models to search in
scope = list(lower = formula(model.empty), upper = formula(model.full))

# Forward variable selection process
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)
xtable(summary(forwardAIC))

# The MSE on the test data set
pred <- predict(forwardAIC,movie_test)
mean((movie_test$imdb_score-pred)^2)

# Plots
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)

# Variable importance
lr.varimp.step <- data.frame(varImp(forwardAIC));lr.varimp.step
lr.varimp.step$Vars<-row.names(lr.varimp.step);lr.varimp.step$Vars
lr.varimp.step <- lr.varimp.step[order(lr.varimp.step$Overall, decreasing=TRUE),];lr.varimp.step

# Plot variance importance
par(mar=c(4,10,2,6))
barplot(lr.varimp.step$Overall,horiz=TRUE, col="deepskyblue4",xlim=c(0, 15),
        names.arg=lr.varimp.step$Vars, las=1)

# Compare full and reduced regression with anova
anova(forwardAIC,model.full)

# -----------------------------------------------------------------------------------------------

# Regressor matrix
x = as.matrix(pred_movies[, -ncol(pred_movies)])
head(x)

# Response vector
y = pred_movies[, ncol(pred_movies)]
head(y)

# -----------------------------------------------------------------------------------------------
# Ridge regression
# -----------------------------------------------------------------------------------------------

# Get color for plots
display.brewer.all()
cols<-brewer.pal(n=9,name="Blues")

# Scale the data
stan.x <- scale(x)

# Fitting the ridge regression, alpha = 0
grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(stan.x, y, alpha = 0, lambda = grid, standardize = TRUE)

# Inspecting the various coefficient estimates
coef(ridge.models) 

# Visualizing the ridge regression shrinkage.
plot(ridge.models, col=cols, ylab = 'Standardized Coefficients', xvar = "lambda", xlab='Log (lambda)')

# Create training and testing sets
set.seed(0)
test = (-train)
y.test = y[test]

# Running a 10-fold cross validation to choose best lambda
set.seed(0)
cv.ridge.out = cv.glmnet(stan.x[train, ], y[train], lambda = grid, alpha = 0, nfolds = 10)

# Plot of MSE
plot(cv.ridge.out, col="deepskyblue4",xlab='Log (lambda)')

# Show best lambda
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)

# The test MSE associated with the best value of lambda
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = stan.x[test, ]);
mean((ridge.bestlambdatrain - y.test)^2)

# Refit the ridge regression on the overall dataset using the best lambda value
ridge.out = glmnet(stan.x, y, alpha = 0)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)

# The MSE of final ridge model on the entire data set
ridge.bestlambda = predict(ridge.out, s = bestlambda.ridge, newx = stan.x)
coef.ridge <- predict(ridge.out, type = "coefficients", s = bestlambda.ridge,newx = stan.x);coef.ridge
mean((ridge.bestlambda - y)^2)

# Variable importance
varimp.ridge <- data.frame(varImp(ridge.out, lambda=bestlambda.ridge));varimp.ridge
varimp.ridge$Vars<-row.names(varimp.ridge);varimp.ridge$Vars
varimp.ridge <- varimp.ridge[order(varimp.ridge$Overall, decreasing=TRUE),];varimp.ridge

# Plot variable importance
par(mar=c(4,10,2,6))
barplot(varimp.ridge$Overall,horiz=TRUE, col="deepskyblue4",
        names.arg=varimp.ridge$Vars, las=1)

# -----------------------------------------------------------------------------------------------
# Lasso regression
# -----------------------------------------------------------------------------------------------

# Fitting the lasso regression, alpha = 1
grid = 10^seq(5, -2, length = 100)
lasso.models = glmnet(stan.x, y, alpha = 1, lambda = grid)

# The coefficient estimates
coef(lasso.models) 

# Plot the Lasso model
plot(lasso.models, ylab='Standardized Coefficients', col=cols, xvar = "lambda", xlab='Log (lambda)', xlim=c(-4,2))

# Perform cross-validation in order to choose the best lambda
set.seed(0)
cv.lasso.out = cv.glmnet(stan.x[train, ], y[train], lambda = grid, alpha = 1, nfolds = 10)

# Plot MSE
plot(cv.lasso.out, col='blue',xlim=c(-4,1), xvar = "lambda", xlab='Log (lambda)')
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

# The test MSE associated with this best value of lambda
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = stan.x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

# Refit the lasso regression on the overall dataset using the best lambda value
lasso.out = glmnet(stan.x, y, alpha = 1)
coef.lasso <- predict(lasso.out, type = "coefficients", s = bestlambda.lasso,newx = stan.x);coef.lasso

# The MSE of the final lasso model on the entire data set
lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = stan.x)
mean((lasso.bestlambda - y)^2)

# Variable importance
varimp.lasso <- data.frame(varImp(lasso.out, lambda=bestlambda.lasso));varimp.lasso
varimp.lasso$Vars<-row.names(varimp.lasso);varimp.lasso$Vars
varimp.lasso <- varimp.lasso[order(varimp.lasso$Overall, decreasing=TRUE),];varimp.lasso

par(mar=c(4,10,2,6))
barplot(varimp.lasso$Overall,horiz=TRUE, col="deepskyblue4",
        names.arg=varimp.lasso$Vars, las=1)

# -----------------------------------------------------------------------------------------------
# Regression tree
# -----------------------------------------------------------------------------------------------
require(rpart)

# Construct regression tree
set.seed(3)
rt <- rpart(imdb_score~., data=movie_train);rt
summary(rt)

# Plot regression tree
rpart.plot(rt,digits = 3,cex=0.8,tweak=1)

# Predict movie rating based on test set
pred_rt <- predict(rt,movie_test)

# MSE
mean((pred_rt-movie_test$imdb_score)^2);

# Plot cross-validation results
plotcp(rt,col='deepskyblue4',cex=0.8,cex.lab=0.8,cex.axis=0.8)
ylab = 'Relative Error'
axis(side, at=, labels=, pos=, lty=, col=, las=, tck=, ...) 
printcp(rt)

# Prune tree
rt.pruned <- prune(rt, cp = 0.012)
summary(rt.pruned)

# Plot pruned tree
rpart.plot(rt.pruned, digits = 3,cex=0.8,tweak=1)

# Predict movie rating based on test set
pred_rt.pruned <- predict(rt.pruned,movie_test)

# MSE
mean((pred_rt.pruned-movie_test$imdb_score)^2);

# Correlation
cor(pred_rt,movie_test$imdb_score)

# -----------------------------------------------------------------------------------------------
# Random Forest 
# -----------------------------------------------------------------------------------------------

# Perform Random Forest
set.seed(5)
rf <- randomForest(imdb_score~., data=movie_train, ntree=500, proximity = T, do.trace = T)
pred_rf <- predict(rf,movie_test)
mean((pred_rf-movie_test$imdb_score)^2)

# MSE=0.570778

# Importance measure of predictor values
importance(rf)

# Plot importance measure
varImpPlot(rf)

# Random Forest for different number of trees
array_ntree<- c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
mse <- c()
j<-1
for(i in array_ntree)
{ set.seed(5)
  rf <- randomForest(imdb_score~.,data=movie_train,ntree=i, proximity = T, do.trace = T)
  pred_rf <- predict(rf,movie_test)
  mse[j]<-mean((pred_rf-movie_test$imdb_score)^2)
  j=j+1
}

# Table with number of trees and MSE
data_mse <- data.frame(array_ntree,mse)

# Plot MSE for different number of trees
plot(data_mse$array_ntree,data_mse$mse,pch=20,col="deepskyblue4",type="b", main="", xlab="Number of trees", ylab="Mean Squared Error")

# Optimal number of trees
opt_n_tree <- data_mse$array_ntree[data_mse$mse==min(data_mse$mse)]
opt_n_tree

# Solve for optimal tree number
set.seed(5)
rf_new <- randomForest(imdb_score~.,data=movie_train,ntree=opt_n_tree)
pred_rf_new <- predict(rf_new,pred_movies[-train,])
mean((pred_rf_new-pred_movies[-train,]$imdb_score)^2)

# Importance measure of predictor values
importance(rf_new)

# Plot importance measure
varImpPlot(rf_new, main='',cex=0.8)

# -----------------------------------------------------------------------------------------------
