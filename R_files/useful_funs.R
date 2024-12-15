

fitval <- function(X,y,ibss_output) {
  return(X%*% rowSums(ibss_output$mu1 * ibss_output$alpha)+ibss_output$intercept)
}

Xr_cal <- function(X, y, ibss_output) {
  return(fitval(X,y,ibss_output)-mean(y))
}