source("src/simulation.R")
source("src/estimation.R")

n=1000

# Classic spiral

spi = spiral(n, p=2)
flat = LLE_ML(spi[,-4], 2, 13)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(spi[,4]))

# Spiral with noise

spi = spiral(n, p=2, noise=2)
flat = LLE_ML(spi[,-4], 2, 10)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(spi[,4]))

# Sphere

sph = sphere(n)
flat = LLE_ML(sph[,-4], 2, 8)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(sph[,4]))

wav = waves(n)
flat = LLE_ML(wav[,-4], 2, 10)
plot(flat$Y[,1], flat$Y[,2], col=myColorRamp(wav[,4]))
