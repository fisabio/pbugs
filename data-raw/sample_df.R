
set.seed(12345)
ilogit    <- function(x) exp(x) / (1 + exp(x))
N         <- 1000
x1        <- rbinom(N, 1, prob = .7)
x2        <- rbinom(N, 1, prob = .4)
x3        <- rnorm(N, 40, 10)
x3        <- (x3 - mean(x3)) / sd(x3)
betas     <- matrix(c(-2, 1.3, 1.05, .04))
y         <- rbinom(N, 1, prob = ilogit(cbind(rep(1, N), x1, x2, x3) %*% betas))
sample_df <- data.frame(x1, x2, x3, y)
