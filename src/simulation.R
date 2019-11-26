library(rgl)
require(pdist)

myColorRamp <- function(values, colors=c("red", "yellow", "green", "cyan", "blue")) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

addNoise <- function(data, value) {
  if (value > 0) {
    n = nrow(data)
    p = ncol(data)
    data = data + matrix(runif(n*p, 0, value), n, p)
  }
  return(data)
}

# n : Nombre de points
# p : Nombre de tours
spiral <- function(n, p=2, noise=0) {
  X = runif(n, 0, 4 * pi * p)
  Y = runif(n, 0, 2 * pi * p)
  Y_spiral = Y * cos(Y)
  Z_spiral = Y * sin(Y)
  data = cbind(X, Y_spiral, Z_spiral)
  data = addNoise(data, noise)
  data = cbind(data, Y)
  manifold = cbind(X, Y)
  return(list(data=data, manifold=manifold))
}

# n : Nombre de points
sphere <- function(n, noise=0) {
  A = runif(n, 0, 2 * pi)
  B = runif(n, -pi/2, pi/2)
  X = cos(A) * cos(B)
  Y = sin(A) * cos(B)
  Z = sin(B)
  data = cbind(X,Y,Z)
  data = addNoise(data, noise)
  data = cbind(data, A)
  manifold = cbind(A, B)
  return(list(data=data, manifold=manifold))
}

waves <- function(n, a=1, b=1, c=1, d=1, noise=0) {
  X = runif(n, 0, 2*pi)
  Y = runif(n, 0, 2*pi)
  Z = a * sin(b * X) + c * sin(d * Y)
  data = cbind(X, Y, Z)
  data = addNoise(data, noise)
  data = cbind(data, X)
  manifold = cbind(X, Y)
  return(list(data=data, manifold=manifold))
}

## 3-sensor data set ####
## Artificial data as from P. Desmartines, PhD Tesis 1994
data_10D <- function(n, noise=0) {
  
  # these sensors where selected randomly
  sensors <- matrix(ncol = 3, data = 
                      c(0.026, 0.236, -0.653, 0.310, 0.507, -0.270, -0.466,  -0.140, 0.353, -0.473,
                        0.241, 0.193, 0.969, 0.094, 0.756, -0.978, -0.574, -0.502, -0.281, 0.993,
                        0.026, -0.913, -0.700, 0.876, 0.216, -0.739, 0.556, -0.155, 0.431, 0.411))
  
  # draw random points on the 3d unit cube
  manifold <- matrix(runif(3 * n, -1, 1), ncol = 3)
  
  # We ode each point as the distance to sensors : intrinsic dimension = 3
  # while extrinsic dimension = 10
  data <- as.matrix(pdist(manifold, sensors))
  data <- addNoise(data, noise)
  return(list(data=data, manifold=manifold))
}
