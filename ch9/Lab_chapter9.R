set.seed(1)
x = matrix(rnorm(20*2), ncol=2)
y=c(rep(-1, 10), rep(1, 10))
x[y == 1, ]=x[y == 1, ] + 1
plot(x, col=(3-y))

# Train svm
dat=data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit = svm(y~., data=dat, kernel="linear", cost=10, scale=F)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

# Tune svm
set.seed(1)
tune.out = tune(svm, y~., data=dat, kernel="linear", 
                ranges=list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Predict
xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1, 1), 20, replace=T)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdat = data.frame(x=xtest, y=as.factor(ytest))
ypred = predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

# Create linearly separable classes and plot them
x[y==1 ,]= x[y==1 ,]+0.5
plot(x, col =(y+5) /2, pch =19)

# Train svm with very large value of cost to create separating hyperplane
dat=data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)

# Full support vector machines with radial basis kernels
# Generate Data
set.seed (1)
x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(x=x, y=as.factor(y))
plot(x, col=y)
train = sample(200, 100)
svmfit = svm(y~., data=dat[train, ], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train, ])
summary(svmfit)

# Cross validation using SVM
set.seed(1)
tune.out = tune(svm, y~., data=dat[train, ], kernel="radial", 
                range=data.frame(cost=c(0.1, 1, 10, 100, 100), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)
tune.pred = predict(tune.out$best.model, dat[-train, ])
table(dat[-train, "y"], tune.pred)

# Generate ROC curve
library(ROCR)
rocplot = function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
svmfit.opt = svm(y~., data=dat[train ,], kernel ="radial", gamma=2, cost=1, decision.values=T)
fitted = attributes(predict(svmfit.opt, dat[train, ], decision.values=T))$decision.values
par(mfrow=c(1, 2))
rocplot(fitted, dat[train, "y"], main="Training data")
# Plot by increasing lamdba
svmfit.flex = svm(y~., data=dat[train, ], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted = attributes(predict(svmfit.flex, dat[train, ], decision.values=T))$decision.values
rocplot(fitted, dat[train, "y"], col="red", add=T)

# Make same plots on test data
fitted = attributes(predict(svmfit.opt, dat[-train, ], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted = attributes(predict(svmfit.flex, dat[-train, ], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], col="red", add=T)

# SVM with multiple classes
set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol=2))
y = c(y, rep(0, 50))
x[y==0, 2] = x[y==0, 2] + 2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1, 1))
plot(x, col=(y+1))
svmfit = svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# SVM on gene expression data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

dat = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out = svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)
dat.test = data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred = predict(out, dat.test)
table(pred, dat.test$y)