
set.seed(12345)
N         <- 1000
x1        <- rbinom(N, 1, prob = .7)
x2        <- rbinom(N, 1, prob = .4)
x3        <- scale(rnorm(N, 40, 10), center = TRUE, scale = FALSE)
y         <- rbinom(N, 1, prob = plogis(tcrossprod(cbind(rep(1, N), x1, x2, x3), t(c(-2, 1.3, 1.05, .04)))))
sample_df <- data.frame(x1, x2, x3, y)
