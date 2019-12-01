library("jpeg")

folder = ".../faces"

flatten <- function(img) {
  return(c(img))
}

remove_doubles <- function(mat) {
  d = as.matrix(dist(mat))
  to_remove=c()
  for (i in (nrow(d)-1):1) {
    for (j in ncol(d):(i+1)) {
      if (d[i,j]==0)
        to_remove=c(to_remove, i)
    }
  }
  mat[-to_remove,]
}

images = c()
for (i in list.files(folder)) {
  img = flatten(readJPEG(paste(c(folder, i), collapse="/")))
  images = rbind(images, img)
}
images=remove_doubles(images)

s=5
trust_K=seq(from=1, to=75, length.out=50)
sigmas = seq(from=0.0001, to=0.001, length.out=20)

KPCAs = lapply(sigmas, function(i) KPCA_ML(images, s, i))
comp_kpca = compare(images, KPCAs, trust_K)
plot_compare(comp_kpca, sigmas)

k_iso = seq(from=4, to=20, by=2)
isos = lapply(k_iso, function(i) isomap_ML(images, s, i)$points)
comp = compare(images, isos, trust_K)
plot_compare(comp_iso, k_iso)

k_lle = seq(from=4, to=20, by=2)
lles = lapply(k_lle, function(i) LLE_ML(images, s, i)$Y)
comp_lle = compare(images, lles, trust_K)
plot_compare(comp_lle, k_lle)

f = full_comparison(images, 20, k_iso=7, k_lle=13, sig_kpca=0.025, trust_K=seq(from=1, to=75, length.out=50))
plot_compare(f$comparison, f$legend)
