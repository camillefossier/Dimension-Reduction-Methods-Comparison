source("src/simulation.R")
source("src/estimation.R")

n = 1000

X <- generateData(n)
classical_MDS(X, 3)

spi = spiral(n, p=2)
plot3d(spi[,1], spi[,2], spi[,3], col=myColorRamp(spi[,4]), aspect=F)
flat = classical_MDS(spi[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi[,4]))

sph = sphere(n)
flat = classical_MDS(sph[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(sph[,4]))

wav = waves(n)
flat = classical_MDS(wav[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(wav[,4]))

# TODO : Try to use it on the city distances to see the results