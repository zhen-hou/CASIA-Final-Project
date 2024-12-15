library(susieR)

data(N2finemapping)

set.seed(777)
X <- matrix(rnorm(100 * 100), 100, 100)
beta <- rep(0, 100)
beta[1 : 10] <- 1
y <- X %*% beta + rnorm(100)
s = susie_auto(X,y,20)

print(1)

plot(coef(s)[2:11],beta[1:10])