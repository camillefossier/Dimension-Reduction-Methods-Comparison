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
  kpc = KPCA_ML(high, s, sig)
  
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
  
  K=seq.int(from=1, to=400, length.out=30)
  comp = compare(high, lows, K)
  
  list(
    lows=lows,
    comparison=comp,
    legend=legend
  )
}

n=1000

spi = spiral(n, p=2)
plot3d(spi$data[,1], spi$data[,2], spi$data[,3], col=myColorRamp(spi$data[,4]), aspect=F)
f = full_comparison(spi$data[,-4], 2, k_iso=7, k_lle=13, sig_kpca=0.025)

layout(matrix(c(1,2,3,4,5,6),2,3))
for (l in seq_along(f$lows)) {
  plot(f$lows[[l]][,1], f$lows[[l]][,2], col=myColorRamp(spi$data[,4]), sub=f$legend[l], xlab="x", ylab="y")
}
plot_compare(f$comparison, f$legend)
