source('src/simulation.R')

n=100000

spi = spiral(n, p=2, noise=0)
plot3d(spi$data[,1], spi$data[,2], spi$data[,3], col=myColorRamp(spi$data[,4]), aspect=F)
plot(spi$manifold[,1], spi$manifold[,2], col=myColorRamp(spi$manifold[,3]))

sph = sphere(n, noise=0.2)$data
plot3d(sph[,1], sph[,2], sph[,3], col=myColorRamp(sph[,4]), aspect=F)

wav = waves(n)$data
plot3d(wav[,1], wav[,2], wav[,3], col=myColorRamp(wav[,4]), aspect=F)
