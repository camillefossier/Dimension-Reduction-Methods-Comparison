source("src/simulation.R")
source("src/estimation.R")

n=1000

spi = spiral(n)
sigmas=seq(from=0.00001, to=0.025, length.out=10)

flats <- lapply(sigmas, function(sig) KPCA_ML(spi$data[,-4], 2, sigma=sig))

layout(matrix(c(1:12), 3, 4))
for (flat in flats) {
  plot((flat), col=myColorRamp(spi$data[,4]))
}
comp = compare(spi$data[,-4], flats, seq(1,300,20))
plot_compare(comp, legend=sigmas)
