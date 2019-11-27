source("src/simulation.R")
source("src/estimation.R")
source("src/evaluation.R")

n=1000

spi = spiral(n, p=2, noise=2)
plot3d(spi$data[,1], spi$data[,2], spi$data[,3], col=myColorRamp(spi$data[,4]), aspect=F)

mds = classical_MDS(spi$data[,-4], 2)
sam = sammon_NLM(spi$data[,-4], 2)
iso = isomap_ML(spi$data[,-4], 2, 7)
lle = LLE_ML(spi$data[,-4], 2, 13)

lows = list(
  mds=mds$points,
  sam=sam$points,
  iso=iso$points,
  lle=lle$Y
)
legend = c(
  "MDS",
  "Sammon",
  "Isomap",
  "LLE"
)

K=seq.int(from=1, to=300, length.out=20)
comp = compare(spi$data[,-4], lows, K, legend)
