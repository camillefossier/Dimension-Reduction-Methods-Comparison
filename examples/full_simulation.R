source("src/simulation.R")
source("src/estimation.R")
source("src/evaluation.R")

n=1000

spi = spiral(n, p=2)
plot3d(spi$data[,1], spi$data[,2], spi$data[,3], col=myColorRamp(spi$data[,4]), aspect=F)

# Predict #

mds = classical_MDS(spi$data[,-4], 2)
sam = sammon_NLM(spi$data[,-4], 2)
iso = isomap_ML(spi$data[,-4], 2, 7)
lle = LLE_ML(spi$data[,-4], 2, 13)
pca = predict(PCA(spi$data[,-4]), spi$data[,-4])

high = spi$data[,-4]
value = spi$data[,4]

lows = list(
  mds=mds$points,
  sam=sam$points,
  iso=iso$points,
  lle=lle$Y,
  pca=pca$coord[,1:2]
)

legend = c(
  "MDS",
  "Sammon",
  "Isomap",
  "LLE",
  "PCA"
)

# Plot #

layout(matrix(c(1,2,3,4,5,6),2,3))

for (l in seq_along(lows)) {
  plot(lows[[l]][,1], lows[[l]][,2], col=myColorRamp(value), sub=legend[l], xlab="x", ylab="y")
}

# Evaluation #

K=seq.int(from=1, to=400, length.out=30)
comp = compare(high, lows, K)
plot_compare(comp, legend)
