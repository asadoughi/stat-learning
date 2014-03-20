n.samples = 300
y = sample(c(0, 1), n.samples, replace=T)
x = matrix(rep(0, n.samples * 10), ncol=10)

for (i in 1:n.samples) {
  if (y[i] == 0) 
    x[i, ] = rnorm(10)
  else
    x[i, ] = rnorm(10, mean=c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
}

total.0 = seq(1, n.samples)[y == 0]
total.1 = seq(1, n.samples)[y == 1]
train.0 = sample(total.0, 50)
train.1 = sample(total.1, 50)
train = c(train.0, train.1)

library(e1071)

dat = data.frame(x=x, y=as.factor(y))
svm.fit = svm(y~., data=dat[train, ], kernel="linear")
svm.pred = predict(svm.fit, dat[-train, ])
mean(dat[-train, "y"] != svm.pred)

glm.fit = glm(y~., dat[train, ], family=binomial)
glm.prob = predict(glm.fit, dat[-train, ], type="response")
glm.pred = ifelse(glm.prob > 0.5, 1, 0)
sum(dat[-train, "y"] != glm.pred)