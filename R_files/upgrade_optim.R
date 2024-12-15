upgrade_optim <- function(X,y,sigma2,xtx_vector){
  p=ncol(X)
  
  upbound <- 0.1
  #print(y[1:10])
  myterm <- 0
  if (TRUE) {
    for (j in 1:p){
    upgradeterms <- bf_mu1_sigma12(X[,j],y,sigma2,1, 
                                   xtx_vector[j])
    #not affected by sigma02
    upbound <- max(upbound, upgradeterms$term)
    
    myterm <- myterm + upgradeterms$term/p
    }
  }
  
  xty_vector <- rep(0, p)
  for (j in 1:p){
    xty_vector[j] <- t(X[,j]) %*% y
  }
  
  
  #print(upbound)
  
  #'upbound' has theoretical guarantee to be an upbound
  #'-upbound' just to eliminate small sigma02,can be change to any negetive number
  # notice ,if choose interval = c(0, upbound), small sigma02 can not be eliminated
  result <- suppressWarnings(optim(fn = function(sigma02) (objective(sigma2,sigma02, 
                                                    xtx_vector,xty_vector)),
                     par = myterm, lower = -50, upper=50 +upbound ,
                  method = "Brent"))
  
  
  #print(result$par)
  
  return(result$par)
  
}

objective <- function(sigma2,sigma02,xtx_vector, xty_vector ){
  val <-0
  p = length(xtx_vector)
  for (j in 1:p){
    upgradeterms <- bf_fast(sigma2,sigma02,
                                   xtx_vector[j],xty_vector[j])
    val <- val + upgradeterms
  }
  return(-val)
}