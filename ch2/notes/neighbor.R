# Graph the technique described by Trevor Hastie in Lecture 2.1

set.seed(37)

# Generate sample data
n <- 20
x0 <- sort(runif(n, min=0, max=10))
y0 <- 0.5 * x0 + rnorm(n, sd=0.2)

Estimate <- function(x, neighborhood=2){
    # Estimates x as the average value of y within neighborhood
    # Note this implementation is not k nearest neighbors,
    # since it uses a neighborhood of fixed size.
    mean(y0[abs(x - x0) < neighborhood])
}

x <- seq(0, 10, length.out=2000)
yhat <- sapply(x, Estimate)

png('neighbor.png')
plot(x, yhat, col='green')
points(x0, y0)
dev.off()
