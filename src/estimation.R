source("src/simulation.R")

#### Dimensionality Estimation ####

  # PCA : See examples/dimensionality_estimation.R (using prcomp for PCA)

  # Correlation dimension, Grassberger and Procaccia (case where q=2)

corrDim <- function(data, epsilon = 10^seq(-2, 1, length.out = 100)){
  distmat <- dist(data)
  # Determine the proportion of distances <= epsilon
  C <- numeric(length(epsilon))
  for(k in seq_along(C)) {
    C[k] <- sum(distmat <= epsilon[k]) #/ (nrow(X) - 1) / nrow(X)
  }
  
  return(list(epsilon = epsilon, C = C))
}

derivate <- function(x, y, smoothing=1) {
  return(c(diff(y, lag=smoothing) / diff(x, lag=smoothing), rep(NA, smoothing)))
}

#### Dimensionality Reduction ####

  # MDS : Multidimensional Scaling

    # Metric MDS

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
classical_MDS <- function(X, s) {
  d <- dist(X) # euclidean distances between the rows
  mds <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
  print(mds) # view results
  
  # plot solution
  x <- mds$points[,1]
  y <- mds$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", type="n")
  text(x, y, labels = row.names(X), cex=.7)
  
  return(mds)
}
  # Sammon's Nonlinear Mapping

  # Graph Distances

    # Geodesic Graph Distance

  # Data-driven lattice

    # Locally linear embedding