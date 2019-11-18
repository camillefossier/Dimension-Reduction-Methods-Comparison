source("src/simulation.R")
source("src/estimation.R")

#### Using PCA ####

res100   <- generateData(100)
res1000  <- generateData(1000)
res10000 <- generateData(10000)

pr100   <- prcomp(res100)
pr1000  <- prcomp(res1000)
pr10000 <- prcomp(res10000)

plot( pr100$sdev   / pr100$sdev[1]  , lwd = 2, type = 'l', ylab = "Normalized Eigenvalues", xlab = "Dimension")

lines(pr1000$sdev  / pr1000$sdev[1] , lwd = 2, col = 2)
lines(pr10000$sdev / pr10000$sdev[1], lwd = 2, col = 4)

#### Using corr_dim ####

X <- generateData(1000)
Xdim <- corrDim(X, epsilon = 10^seq(-4, 1, length.out = 100))

# Plot C2 vs epsilon (in log-log)
plot(log10(Xdim$epsilon), log10(Xdim$C), type = 'l',xlab = expression(log(epsilon)), ylab = expression(log(C(epsilon))))

layout(matrix(1:3, 1))

# Plot d log(C2) vs d log(epsilon) 
plot(log10(Xdim$epsilon), derivate(log10(Xdim$epsilon), log10(Xdim$C)), type = 'l')
plot(log10(Xdim$epsilon), derivate(log10(Xdim$epsilon), log10(Xdim$C), smoothing=2), type = 'l')
plot(log10(Xdim$epsilon), derivate(log10(Xdim$epsilon), log10(Xdim$C), smoothing=3), type = 'l')
