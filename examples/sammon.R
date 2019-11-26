source("src/simulation.R")
source("src/estimation.R")

n=1000

spi = spiral(n, p=2)
flat = sammon_NLM(spi[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi[,4]))

sph = sphere(n)
flat = sammon_NLM(sph[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(sph[,4]))

wav = waves(n)
flat = sammon_NLM(wav[,-4], 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(wav[,4]))
