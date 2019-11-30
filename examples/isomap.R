source("src/simulation.R")
source("src/estimation.R")

n=1000

# Classic spiral

spi = spiral(n, p=2)
flat = isomap_ML(spi$data[,-4], 2, 7)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi$data[,4]))

# Spiral with noise

spi = spiral(n, p=2, noise=2)
flat = isomap_ML(spi$data[,-4], 2, 5)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi$data[,4]))

# Sphere

sph = sphere(n)
flats = lapply(4:9, function(i) {
  print(i)
  isomap_ML(sph$data[,-4], 2, i)
})
layout(matrix(c(1,2,3,4,5,6), 2, 3))
for (flat in flats) {
  plot(flat$points[,1], flat$points[,2], col=myColorRamp(sph$data[,4]))
}

wav = waves(n)
flat = isomap_ML(wav$data[,-4], 2, 10)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(wav$data[,4]))
