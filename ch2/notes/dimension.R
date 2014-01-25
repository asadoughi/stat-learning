# This script creates plots illustrating the curse of dimensionality

# Choose points, randomly distributed
n <- 3000  # number of points
p <- 2  # number of dimensions

x <- matrix(runif(n*p), ncol=p)

Fringe <- function(z){
  # Determine if z lies within 0.05 distance from the boundary
  # z is a vector of length p
  any(abs(z - 0.5) > 0.45)
}

result <- apply(x, 1, Fringe)

png('edges.png')
plot(x[,1:2])
points(x[,1][result], x[,2][result], col='red')
dev.off()

ratio <- sum(result) / n

# plot p versus ratio
p <- 1:50

GetRatio <- function(dim){
  1 - (0.9)^dim
}
ratio <- sapply(p, GetRatio)

png('curse_dimensionality.png')
plot(p, ratio)
dev.off()
