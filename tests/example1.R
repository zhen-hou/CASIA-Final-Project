n = 500
p = 200
trueL = 6
L = trueL + 5

cor_true = 0.95

num_similar = 3

mixed = TRUE
cor_low = 0.3
cor_up = 0.999



if (mixed) {
  cors <- seq(cor_low,cor_up,length.out = trueL)
} else {
  cors <- rep(cor_true, trueL)
}


set.seed(7)
effect_similar <- matrix(sample(1:p, trueL*(num_similar+1), replace = FALSE),
                         nrow = trueL)
X <- matrix(rnorm(n * p), n, p)
beta <- rep(0, p)
beta[effect_similar[,1]] <- 1

for (i in 1:trueL){
  for (j in 1:num_similar){
    X[,effect_similar[i,j+1]] <- (cors[i] * X[,effect_similar[i,1]] 
                                  + sqrt(1-cors[i]^2) * rnorm(n))
  }
}

y <- X %*% beta + rnorm(n)


output <- ibss(X,y,L)

print(credible_sets(output,X))

print(effect_similar)
