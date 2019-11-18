source("src/simulation.R")
source("src/estimation.R")

X <- generateData(1000)
classical_MDS(X, 3)

spi = spiral(1000, p=1)
flat = classical_MDS(spi, 2)
plot(flat$points[,1], flat$points[,2], col=myColorRamp(spi[,4]))

