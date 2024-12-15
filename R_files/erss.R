erss <- function(y, bbar, b2bar, X){
  term1 <- sum((y-rowSums(X%*%bbar))^2)
  
  term2 <- sum(X^2 %*% b2bar) - sum((X%*%bbar)^2)
  
  return(term1 + term2)
}