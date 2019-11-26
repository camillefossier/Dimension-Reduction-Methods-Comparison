source("src/simulation.R")
source("src/estimation.R")

n=1000

spi = spiral(n, p=2)
flat = sammon_NLM(spi$data[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi$data[,4]))

sph = sphere(n)
flat = sammon_NLM(sph$data[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(sph$data[,4]))

wav = waves(n)
flat = sammon_NLM(wav$data[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(wav$data[,4]))
