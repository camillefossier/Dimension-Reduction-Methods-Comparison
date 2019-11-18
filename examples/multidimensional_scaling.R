source("src/simulation.R")
source("src/estimation.R")

n = 1000

X <- generateData(n)
classical_MDS(X, 3)

spi = spiral(n, p=1)
flat = classical_MDS(spi, 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi[,4]))

sph = sphere(n)
flat = classical_MDS(sph, 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(wav[,4]))

wav = waves(n)
flat = classical_MDS(wav, 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(sph[,4]))

# TODO : Try to use it on the city distances to see the results