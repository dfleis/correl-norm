set.seed(124)

##### params ##### 
nsims <- 1e2
nrv <- 2

##### generate correlation matrix R and variances V #####
sigma <- rep(1, nrv)
R <- matrix(0, nrow = nrv, ncol = nrv)
diag(R) <- 1
R[lower.tri(R, diag = F)] <- rep(-0.99, nrv * (nrv - 1)/2) #runif(nrv * (nrv - 1) / 2, -1, 1)
R[upper.tri(R, diag = F)] <- R[lower.tri(R, diag = F)]

##### given correlation matrix R and variances V, compute the var-cov matrix SIGMA #####
D <- matrix(0, nrow = nrv, ncol = nrv)
diag(D) <- sigma
SIGMA <- D %*% R %*% D

##### use cholesky decomposition to find our matrix C such that C^T C = SIGMA #####
C <- chol(SIGMA)

##### generate correlated normal deviates #####
Z <- matrix(rnorm(nsims * nrv), ncol = nrv)
Y <- Z %*% C


###### tests ######

idx1 <- 1
idx2 <- 2
cor(Y)

plot(Y[,idx1], Y[,idx2])


y1 <- seq(min(Y[,idx1]), max(Y[,idx1]), length.out = length(Y[,idx1]))
y2 <- seq(min(Y[,idx2]), max(Y[,idx2]), length.out = length(Y[,idx2]))

L <- lapply(y2, FUN = function(x2) sapply(y1, FUN = function(x1) mean(Y[,idx1] <= x1 & Y[,idx2] <= x2)))
M <- matrix(unlist(L), ncol = length(y2))
image(y1, y2, M)
contour(y1, y2, M)





















