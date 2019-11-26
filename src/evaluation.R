trustworthiness <- function(data, manifold, k) {
  n = nrow(data)
  trust = 0
  # distances des points à i en faible dimension
  d_m = as.matrix(dist(manifold))
  # distances des points à i en haute dimension
  d_d = as.matrix(dist(data))
  for (i in 1:n) {
    d_m_i = d_m[,i]
    d_d_i = d_d[,i]
    # indices des points proches en faible dimension
    o_m = order(d_m)
    # indice des points proches en hautes dimensions
    o_d = order(d_d)
    u = setdiff(o_m[1:(k+1)], o_d[1:(k+1)])
    if (length(u)>0) print(u)
    for (j in u) {
      
      trust = trust + (j - k)
      
    }
  }
  trust
}