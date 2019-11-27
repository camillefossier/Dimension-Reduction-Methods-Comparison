source("src/estimation.R")

trustworthiness <- function(high, low, k, dh=NULL, dl=NULL, order=T) {
  n = nrow(high)
  # distances des points à i en faible dimension
  if (is.null(dl)) dl = as.matrix(dist(low))
  # distances des points à i en haute dimension
  if (is.null(dh)) dh = as.matrix(dist(high))
  trustworthiness = 0
  for (i in 1:n) {
    # indices des points proches en faible dimension
    ol = dl[,i]
    if (order) ol = order(ol)
    # indice des points proches en hautes dimensions
    oh = dh[,i]
    if (order) oh = order(oh)
    u = setdiff(ol[1:(k+1)], oh[1:(k+1)])
    for (j in u) {
      
      trustworthiness = trustworthiness + (j - k)
      
    }
  }
  1 - 2 * trustworthiness / (n*k * (2*n - 3*k - 1))
}

continuity <- function(high, low, k, dh=NULL, dl=NULL, order=T) {
  n = nrow(high)
  # distances des points à i en faible dimension
  if (is.null(dl)) dl = as.matrix(dist(low))
  # distances des points à i en haute dimension
  if (is.null(dh)) dh = as.matrix(dist(high))
  continuity = 0
  for (i in 1:n) {
    # indices des points proches en faible dimension
    ol = dl[,i]
    if (order) ol = order(ol)
    # indice des points proches en hautes dimensions
    oh = dh[,i]
    if (order) oh = order(oh)
    u = setdiff(oh[1:(k+1)], ol[1:(k+1)])
    for (j in u) {
      continuity = continuity + (j - k)
    }
  }
  1 - 2 * continuity / (n*k * (2*n - 3*k - 1))
}

compare <- function(high, lows, K) {
  trust = matrix(NA, nrow=length(K), ncol=0)
  conti = matrix(NA, nrow=length(K), ncol=0)
  dh = as.matrix(dist(high))
  dh = apply(dh, 2, order)
  for (low in lows) {
    dl = as.matrix(dist(low))
    dl = apply(dl, 2, order)
    trust = cbind(trust, unlist(lapply(K, function(k) trustworthiness(high, low, k, dh=dh, dl=dl, order=F))))
    conti = cbind(conti, unlist(lapply(K, function(k) continuity(high, low, k, dh=dh, dl=dl, order=F))))
  }
  
  list(trustworthiness=trust, continuity=conti)
}

plot_compare <- function(compare, legend) {
  layout(t(c(1,2)))
  matplot(K, compare$trustworthiness, type="l", lty=1, lwd=2, col=seq_along(lows), xlab="Neighbors", ylab="Trustworthiness")
  legend("bottomright", legend=legend, col=seq_along(lows), pch=1) # optional legend
  
  matplot(K, compare$continuity, type="l", lty=1, lwd=2, col=seq_along(lows), xlab="Neighbors", ylab="Continuity")
  legend("bottomright", legend=legend, col=seq_along(lows), pch=1) # optional legend
}
