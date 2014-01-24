# Create a vector
x <- c(1, 3, 2, 5)
x
# Can use = instead of <-
y = c(1, 4, 3, 9)
# Length and other operations on vectors
length(x)
length(y)
x+y
# Finding all objects in workspace
ls()
# Remove some of the objects
rm(x)
rm(list=ls())
ls()
# Help function
?matrix
# Create Matrix
x=matrix(data=c(1, 2, 3, 4), nrow=2, ncol=2)
x
# Same as above
x=matrix(c(1, 2, 3, 4), 2, 2)
# Default fills the matrix in column order, pass byrow=TRUE to enable row order
matrix(c(1, 2, 3, 4), 2, 2, byrow=TRUE)
# Operations on matrix
sqrt(x)
x^2
# Random number generators
x=rnorm(50)
y=x+rnorm(50, mean=50, sd=0.1)
cor(x, y)
# Setting seed for random number generation. This produces same set of random numbers
set.seed(134)
rnorm(60)
# More vector operations: Mean, Variance
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
# Sequences
x=seq(1, 10)
x
x=1:10
x
x=seq(-pi, pi, length=50)
x

# Graphics: R has some of best plotting support
x=rnorm(100)
y=rnorm(100)
plot(x, y)
plot(x, y, xlab="this is x-axis", ylab="this is y-axis", main="Plot of X vs Y")
# Savinf plot output
# We can use 'export' menu in the Plot window
pdf("Figure.pdf")
plot(x, y, col="green")
dev.off()
# More sophisticated plots
?contour
y=x
f=outer(x, y, function(x, y) cos(x)/(1+x^2))
contour(x, y, f)
contour(x, y, f, nlevels=45, add=T)
fa=(f-t(f))/2
contour(x, y, fa, nlevels=15)
?image
image(x, y, fa)
?persp
persp(x, y, fa)
persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=20)
persp(x, y, fa, theta=30, phi=70)
persp(x, y, fa, theta=30, phi=40)

# Indexing data
A=matrix(1:16, 4, 4)
A
A[2, 3]
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
A[1, ]
# Negative index
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
# Dimensions
dim(A)

# Loading/storing data
help(read.table)
help(write.table)
Auto=read.table ("../data/Auto.data", header=T, na.strings="?")
fix (Auto)
Auto = read.csv ("../data/Auto.csv", header=T, na.strings="?")
fix (Auto)
dim (Auto)
Auto [1:4, ]
# Remove missing values
Auto = na.omit(Auto)
dim(Auto)
# Find column names
names(Auto)

# Accessing and plotting columns
plot (Auto$cylinders, Auto$mpg)
attach (Auto)
plot (cylinders, mpg)
cylinders = as.factor (cylinders)
plot(cylinders , mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ", ylab ="MPG ")

# Histogram
hist (mpg)
hist (mpg, col=2)
hist (mpg, col=2, breaks=15)

 # Pairwise scatterplots
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)

# Identiy prints the name on plot
plot (horsepower, mpg)
identify (horsepower, mpg, name)

# Summarizing dataframe
summary (Auto)
aummary (mpg)

 # Finished. quit
q()
