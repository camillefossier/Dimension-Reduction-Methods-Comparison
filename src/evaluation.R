trustworthiness <- function(high, low, k) {
  n = nrow(high)
  # distances des points à i en faible dimension
  dl = as.matrix(dist(low))
  # distances des points à i en haute dimension
  dh = as.matrix(dist(high))
  trustworthiness = 0
  for (i in 1:n) {
    # indices des points proches en faible dimension
    ol = order(dl[,i])
    # indice des points proches en hautes dimensions
    oh = order(dh[,i])
    u = setdiff(ol[1:(k+1)], oh[1:(k+1)])
    for (j in u) {
      
      trustworthiness = trustworthiness + (j - k)
      
    }
  }
  1 - 2 * trustworthiness / (n*k * (2*n - 3*k - 1))
}

continuity <- function(high, low, k) {
  n = nrow(high)
  # distances des points à i en faible dimension
  dl = as.matrix(dist(low))
  # distances des points à i en haute dimension
  dh = as.matrix(dist(high))
  continuity = 0
  for (i in 1:n) {
    # indices des points proches en faible dimension
    ol = order(dl[,i])
    # indice des points proches en hautes dimensions
    oh = order(dh[,i])
    u = setdiff(oh[1:(k+1)], ol[1:(k+1)])
    for (j in u) {
      continuity = continuity + (j - k)
    }
  }
  1 - 2 * continuity / (n*k * (2*n - 3*k - 1))
}

