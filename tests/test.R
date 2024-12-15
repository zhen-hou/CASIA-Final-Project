folder_path <- "~/R_scripts/CASIfinal/R_files"

r_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)

for (file in r_files) {
  source(file)
}

library(susieR)
data("N2finemapping")

n<-1000
p<- 100
L <- 5
trueL <- L - 3



set.seed(777)
X <- matrix(rnorm(n * p), n, p)
beta <- rep(0, p)
beta[(p-trueL): p] <- 1
y <- X %*% beta + rnorm(n)

X[,1] <- X[,p] + 0.01 * rnorm(n)


if(FALSE){
s<- susie(X,y, L=L, scaled_prior_variance = 0.1,
          residual_variance = 0.2,
          estimate_residual_variance = TRUE,
          estimate_prior_variance =TRUE,
          estimate_prior_method = "optim")

s2 <- susie(X,y, L=L, scaled_prior_variance = 0.1,
            residual_variance = 0.2,
            estimate_residual_variance = FALSE,
            estimate_prior_variance =TRUE, 
            estimate_prior_method = "optim")

output <- ibss(X, y, L)

}

upgra <- TRUE

s<- susie(X,y, L=L, scaled_prior_variance = 0.1,
          residual_variance = 0.2,
          estimate_residual_variance = upgra,
          estimate_prior_variance =upgra,
          estimate_prior_method = "optim")

output <- ibss(X, y , 5, 
               upgrade_sigma02 = upgra,
               upgrade_sigma2=upgra, 
               upgrade_sigma02_method = "optim")

print(output$sigma02)
print(s$sigma2)
