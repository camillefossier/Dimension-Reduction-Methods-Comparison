source("src/simulation.R")
source("src/estimation.R")
source("src/evaluation.R")

full_comparison <- function(high, s, k_lle, k_iso, sig_kpca) {

  # Predict #
  
  mds = classical_MDS(high, s)
  sam = sammon_NLM(high, s)
  iso = isomap_ML(high, s, k_iso)
  lle = LLE_ML(high, s, k_lle)
  pca = PCA_ML(high, s)
  kpc = KPCA_ML(high, s, sig_kpca)
  
  lows = list(
    mds=mds$points,
    sam=sam$points,
    iso=iso$points,
    lle=lle$Y,
    pca=pca,
    kpc=kpc
  )
  
  legend = c(
    "MDS",
    "Sammon",
    "Isomap",
    "LLE",
    "PCA",
    "KPCA"
  )
  
  # Evaluation #
  
  K=seq.int(from=1, to=400, length.out=20)
  comp = compare(high, lows, K)
  
  list(
    lows=lows,
    comparison=comp,
    legend=legend
  )
}

n=1000

# SPIRAL #

spi = spiral(n, p=2)
plot3d(spi$data[,1], spi$data[,2], spi$data[,3], col=myColorRamp(spi$data[,4]), aspect=F)
f = full_comparison(spi$data[,-4], 2, k_iso=7, k_lle=13, sig_kpca=0.025)

layout(matrix(c(1,2,3,4,5,6),2,3))
for (l in seq_along(f$lows)) {
  plot(f$lows[[l]][,1], f$lows[[l]][,2], col=myColorRamp(spi$data[,4]), sub=f$legend[l], xlab="x", ylab="y")
}
plot_compare(f$comparison, f$legend)

# SPHERE #

sph = sphere(n)
plot3d(sph$data[,1], sph$data[,2], sph$data[,3], col=myColorRamp(sph$data[,4]), aspect=F)
f = full_comparison(sph$data[,-4], 2, k_iso=7, k_lle=13, sig_kpca=0.025)

layout(matrix(c(1,2,3,4,5,6),2,3))
for (l in seq_along(f$lows)) {
  plot(f$lows[[l]][,1], f$lows[[l]][,2], col=myColorRamp(sph$data[,4]), sub=f$legend[l], xlab="x", ylab="y")
}
plot_compare(f$comparison, f$legend)

# PLANE #

wav = waves(n)
plot3d(wav$data[,1], wav$data[,2], wav$data[,3], col=myColorRamp(wav$data[,4]), aspect=F)
f = full_comparison(wav$data[,-4], 2, k_iso=7, k_lle=13, sig_kpca=0.025)

layout(matrix(c(1,2,3,4,5,6),2,3))
for (l in seq_along(f$lows)) {
  plot(f$lows[[l]][,1], f$lows[[l]][,2], col=myColorRamp(wav$data[,4]), sub=f$legend[l], xlab="x", ylab="y")
}
plot_compare(f$comparison, f$legend)

# FACES #

library("jpeg")

folder = "../faces"

flatten <- function(img) {
  return(c(img))
}

images = c()
for (i in list.files(folder)) {
  img = flatten(readJPEG(paste(c(folder, i), collapse="/")))
  images = rbind(images, img)
}

f = full_comparison(images, 20, k_iso=7, k_lle=13, sig_kpca=0.025)
