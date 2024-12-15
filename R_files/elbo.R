elbo <- function(y, bbar, b2bar, X, sigma2, sigma02, alpha, sigma12) {
  n<- nrow(X)
  p <- ncol(X)
  
  term1 <- - n / 2 *log(2*pi*sigma2)
  
  term2 <- - 1 / (2 * sigma2) * erss(y, bbar,b2bar, X)
  
  term3 <- kl(b2bar, sigma02, alpha, sigma12)
  
  #print(term3)
  
  return(term1+term2 +term3)
  
}

kl <- function(b2bar, sigma02, alpha, sigma12){
  p <- nrow(alpha)
  L <- ncol(alpha)
  
  output <- 0
  
  for (l in 1:L){
    condition_kl <- rep(0, p)
    
    if (sigma02[l]>0){
      for (k in 1:p){
        if (alpha[k,l] > 0){  
          condition_kl[k] <- (log(1/p) - 1/2*log(2*pi*sigma02[l])
                          - (b2bar[k,l])/(2*sigma02[l])
                          -log(alpha[k,l])+1/2*log(2*pi*sigma12[k,l])
                          + 1/2)
          if(is.infinite(condition_kl[k]))
            condition_kl[k] <- 0
        }
      }
    }
    
    
    
    output <- output + sum(alpha[,l]*condition_kl)
  }
  
  return(output)
}