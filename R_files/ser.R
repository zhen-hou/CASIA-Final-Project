ser <- function(X, rlbar, sigma2, sigma02l, 
                upgrade_sigma02_method, sigma02_tol,
                xtx_vector){
 
  n <- nrow(X)
  p <- ncol(X)
  
  alpha <- rep(3,p)
  mu1 <- rep(3,p)
  sigma12 <- rep(3,p) # in paper, no square, remember to ^2
  
  upgrade_sigma02 <- sigma02l
  
  if (upgrade_sigma02_method == "optim"){
    upgrade_sigma02 <- upgrade_optim(X,rlbar,sigma2,xtx_vector)
  }
  
  myterm <-0
  
  for (j in 1:p){
    upgrade <- bf_mu1_sigma12(X[,j],rlbar,sigma2,sigma02l,
                              xtx_vector[j])
    
    alpha[j] <- upgrade$bf #pij = 1/p always, so we omit it
    mu1[j] <- upgrade$mu1
    sigma12[j] <- upgrade$sigma12
    myterm <- myterm + upgrade$term /p
    #print(term)
  }
  #print(alpha[1:5])
  
  if (is.infinite(sum(alpha))){
    inf_count <- sum(is.infinite(alpha))
    alpha[!is.infinite(alpha)] <- 0
    alpha[is.infinite(alpha)] <- 1 / inf_count
  } else {
    alpha <- alpha / sum(alpha)
  }
  
  #print(alpha[1:5])
  
  
  
  if(upgrade_sigma02_method == "mine") {
    upgrade_sigma02 <- myterm
  }
  
  
  if(upgrade_sigma02 < sigma02_tol){
    upgrade_sigma02 <- 0
  }
  
  
  output <- list(
   alphal = alpha,
   mu1l = mu1,
   sigma12l = sigma12,
   sigma02lup = upgrade_sigma02
 ) 
  
 return(output)
  
}

bf_mu1_sigma12 <- function(x,y,sigma2,sigma02,xtx) {
  
  if (sigma02 < 0){
    sigma02 <- 0
  }
  
  #xtx <- t(x) %*% x
  
  bhat <- t(x) %*% y / xtx
  s2 <- sigma2 / xtx
  s <- sqrt(s2)
  z <- bhat / s
  
  #print(z)
  #print(y[1:10])
  #print(t(x)%*%y)
  
  sigma12 <- 1/(1/s2 + 1/sigma02)
  mu1 <- (sigma12 * bhat) / s2
  bf <- sqrt(s2/(sigma02+s2)) * exp((z^2 / 2)*(sigma02/(sigma02+s2)))
  #print(exp((z^2 / 2)))
  
  
 # bf <- min(bf, bf_upbound)
  
  output <- list(
    sigma12 = sigma12,
    mu1 = mu1,
    bf = bf,
    term = bhat^2-s2
  )
  
  return(output)
  
}

bf_fast <- function(sigma2,sigma02,xtx,xty) {
  
  if (sigma02 < 0){
    sigma02 <- 0
  }
  
  #xtx <- t(x) %*% x
  
  bhat <- xty / xtx
  s2 <- sigma2 / xtx
  s <- sqrt(s2)
  z <- bhat / s
  
  #print(z)
  #print(y[1:10])
  #print(t(x)%*%y)
  bf <- sqrt(s2/(sigma02+s2)) * exp((z^2 / 2)*(sigma02/(sigma02+s2)))
  #print(exp((z^2 / 2)))
  
  
  # bf <- min(bf, bf_upbound)
  return(bf)
  
  
}