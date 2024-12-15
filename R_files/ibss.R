ibss <- function(X, y, L,
                 init_sigma2 = 0.2 * var(y),
                 init_sigma02 = rep(0.2*var(y),L),
                 standard = TRUE,
                 init_b = NULL,
                 criterion = 1e-3,
                 max_iter = 1e5,
                 scores = rep(Inf, max_iter),
                 upgrade_sigma2 = TRUE,
                 upgrade_sigma02 = TRUE,
                 upgrade_sigma02_method = "optim",
                 sigma02_tol = 1e-8,
                 output_inv_stand = TRUE,
                 intercept_compute = TRUE
                 ){
  
  n = nrow(X)
  p = ncol(X)
  
  if (!upgrade_sigma02){
    upgrade_sigma02_method <- "no"
  }

  sigma2 = init_sigma2
  sigma02 = init_sigma02
  
  alpha  = matrix(1/p,nrow = p,ncol = L)
  mu1 = matrix(0,nrow = p,ncol = L)
  sigma12 = matrix(1, nrow = p, ncol = L)
  
  if(standard){
    ymean <- mean(y)
    y <- y-ymean
    X <- scale(X)
  } else {
    output_inv_stand = FALSE
    intercept_compute = FALSE
  }
  
  xtx_vector <- rep(0, p)
  for (j in 1:p) {
    xtx_vector[j] <- t(X[,j]) %*% X[,j]
  }
  
  if (is.null(init_b)){
    bbar <- matrix(0, nrow = p, ncol = L)
    for (i in 1:L) {
      bbar[i,i] <- 1
    }
  }
  
  b2bar <- matrix(0,nrow = p, ncol = L)
  
  scores[1] <- -Inf
  
  print("Algorithm starts. The ELBO is:")
  
  for (i in 2:max_iter) {
    rbar <- y - rowSums(X %*% bbar)
    
    for (l in 1:L) {
      rlbar <- rbar + X %*% bbar[,l]
      
      upgrade <- ser(X, rlbar, sigma2, sigma02[l],
                     upgrade_sigma02_method, sigma02_tol,
                     xtx_vector)
      alpha[,l] <- upgrade$alphal
      mu1[,l] <- upgrade$mu1l
      sigma12[,l] <- upgrade$sigma12l
      if (upgrade_sigma02){
        sigma02[l] <- upgrade$sigma02lup
      }
      
      #print(sigma02)
      #print(mu1[1:10,l])
      
      bbar[,l] <- alpha[,l] * mu1[,l]
      b2bar[,l] <- alpha[,l] * (sigma12[,l] + mu1[,l]^2)
      
      
      if (upgrade_sigma02_method == "em"){
        sigma02[l] <- sum(alpha[,l] * b2bar[,l]) #error
        if(sigma02[l]<sigma02_tol){
          sigma02[l] <- 0
        }
      }

      
      rbar <- rlbar - X %*% bbar[,l]
      
    }
    
    if (upgrade_sigma2) {
      sigma2 <- erss(y, bbar, b2bar, X) / n
    }
    
    
    scores[i] <- elbo(y, bbar, b2bar, X, sigma2, sigma02, alpha, sigma12)
    print(scores[i])
    
    if(abs(scores[i]-scores[i-1])<criterion){
      scores <- scores[2:i]
      print("Convergence.")
      break
    }
     
  }
  
  if(intercept_compute){
    intercept <- (ymean 
                  - sum(attr(X, "scaled:center")/ attr(X, "scaled:scale")*alpha*mu1))
    #print(intercept)
  }
  
  if(output_inv_stand){
    mu1 <- mu1 / attr(X, "scaled:scale")
    sigma12 <- sigma12 / (attr(X, "scaled:scale"))^2
  }
  
  
  
  output <- list(
    sigma2 = sigma2,
    sigma02 = sigma02,
    
    alpha  = alpha,
    mu1 = mu1,
    sigma12 = sigma12,
    scores = scores,
    
    intercept = intercept
  )
  
  return(output)
}




