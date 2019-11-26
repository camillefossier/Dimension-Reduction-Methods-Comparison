trustworthiness <- function(data, manifold, k) {
  n = nrow(data)
  trust = 0
  for (i in data) {
    neighbors = NA # TODO
    sort(neighbors) # TODO
    for (n in neighbors) {
      j = neighbors[n]
      trust = trust + (n - k)
      
    }
  }
}