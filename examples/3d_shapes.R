source('src/simulation.R')

spi = spiral(10000, noise=0)
plot3d(spi[,1], spi[,2], spi[,3], col=myColorRamp(spi[,4]), aspect=F)

sph = sphere(10000, noise=0.2)
plot3d(sph[,1], sph[,2], sph[,3], col=myColorRamp(sph[,4]), aspect=F)

wav = waves(20000)
plot3d(wav[,1], wav[,2], wav[,3], col=myColorRamp(wav[,4]), aspect=F)
