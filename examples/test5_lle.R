source("src/simulation.R")
source("src/estimation.R")

n=1000

# Classic spiral

spi = spiral(n, p=2)
flat = LLE_ML(spi$data[,-4], 2, 13)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(spi$data[,4]))

# Spiral with noise

spi = spiral(n, p=2, noise=2)
flat = LLE_ML(spi$data[,-4], 2, 10)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(spi$data[,4]))

# Sphere

sph = sphere(n)
flats = lapply(4:19, function(i) {
  print(i)
  LLE_ML(sph$data[,-4], 2, i)
})
layout(matrix(1:16, 4, 4))
for (flat in flats) {
  plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(sph$data[,4]))
}


wav = waves(n)
flat = LLE_ML(wav$data[,-4], 2, 10)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(wav$data[,4]))
