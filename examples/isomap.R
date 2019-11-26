source("src/simulation.R")
source("src/estimation.R")

n=1000

# Classic spiral

spi = spiral(n, p=2)
flat = isomap_ML(spi[,-4], 2, 7)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi[,4]))

# Spiral with noise

spi = spiral(n, p=2, noise=2)
flat = isomap_ML(spi[,-4], 2, 5)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi[,4]))

# Sphere

sph = sphere(n)
flat = isomap_ML(sph[,-4], 2, 10)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(sph[,4]))

wav = waves(n)
flat = isomap_ML(wav[,-4], 2, 10)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(wav[,4]))
