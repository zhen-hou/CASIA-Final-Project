pip_cal <- function(ibss_output){
  p <- nrow(ibss_output$alpha)
  L <- ncol(ibss_output$alpha)
  result<-rep(1,p)
  
  for (j in 1:p){
    term2 <- 1
    for (l in 1:L){
      if (ibss_output$sigma02[l] > 0){
        term2 <- term2 * (1-ibss_output$alpha[j,l])
      }
    }
    result[j] <- result[j] - term2
  }
  
  return(result)
}

credible_sets<-function(ibss_output,X,rho=0.95, 
                        purity_tol = 0.5, 
                        putity_est = 100,
                        is_sort = TRUE){
  alpha <- ibss_output$alpha
  n <- nrow(X)
  p <- nrow(alpha)
  L <- ncol(alpha)
  
  n_put <- min(n, putity_est)
  
  result <- matrix(0, nrow=p,ncol=L)
  purity <- rep(1,L)
  
  max_num <- 1
  for (l in 1:L){
    alphal <- alpha[,l]
    level <- 0
    num_sig <- 0
    if(ibss_output$sigma02[l] > 0){
      while (level<rho){
        num_sig <- num_sig+1
        result[num_sig,l] <- which.max(alphal)
        level <- level + max(alphal)
        alphal[which.max(alphal)] <- 0
      }
      
      if (num_sig>1){
        exit_outer_loop <- FALSE
        for (k in 1: (num_sig-1)){
          for (m in (k+1):num_sig){
            #print(result[m,l])
            purity[l] <- min(purity[l], 
                             abs(cor(X[1:n_put,result[k,l]], X[1:n_put,result[m,l]])))
            if (purity[l] < purity_tol){
              #print(result)
              result[,l] <-0
              
              exit_outer_loop <- TRUE
              break
            }
          }
          if (exit_outer_loop){
            num_sig <- 0
            break
          }
        }
      }
      max_num <- max(max_num, num_sig)
    } else {
      purity[l] <-0
    }
    
  }
  
  
  result <- result[1:max_num,]
  
  if (max_num==1){
    result <- matrix(result, nrow=1)
  }
  
  #print(result)
  if(is_sort){
    result <- apply(result, 2, sort_with_zeros_last)
  }
  
  
  output <- list(result =result,
                 purity = purity)
  return(output)
}


sort_with_zeros_last <- function(x) {
  if (max(x) == 0){
    return(x)
  }
  sorted_x <- sort(x[x != 0])  
  zeros <- sum(x == 0)    
  if(zeros >0){
    return(c(sorted_x, rep(0, zeros))) 
  } else {
    return(sorted_x)
  }
  
}