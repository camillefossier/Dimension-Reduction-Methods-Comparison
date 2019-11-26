source('src/simulation.R')

n=1000

spi = spiral(n, p=2, noise=2)
plot3d(spi[,1], spi[,2], spi[,3], col=myColorRamp(spi[,4]), aspect=F)

sph = sphere(n, noise=0.2)
plot3d(sph[,1], sph[,2], sph[,3], col=myColorRamp(sph[,4]), aspect=F)

wav = waves(n)
plot3d(wav[,1], wav[,2], wav[,3], col=myColorRamp(wav[,4]), aspect=F)
