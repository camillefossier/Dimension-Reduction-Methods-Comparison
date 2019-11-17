library(rgl)

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
  X = runif(n)
  Y = runif(n, 0, 2 * pi * p)
  Y_spiral = Y * cos(Y)
  Z_spiral = Y * sin(Y)
  data = cbind(X, Y_spiral, Z_spiral, Y)
  data = addNoise(data, noise)
  plot3d(data[,1], data[,2], data[,3], col=myColorRamp(data[,4]))  
}

# n : Nombre de points
sphere <- function(n, noise=0) {
  A = runif(n, 0, 2 * pi)
  B = runif(n, -pi/2, pi/2)
  X = cos(A) * cos(B)
  Y = sin(A) * cos(B)
  Z = sin(B)
  data = cbind(X,Y,Z,A)
  data = addNoise(data, noise)
  plot3d(data[,1], data[,2], data[,3], col=myColorRamp(data[,4]))
}
