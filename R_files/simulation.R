simulation <- function(n = 500,
                       p = 200,
                       trueL = 6,
                       L = trueL + 5,
                       cor_simu = 0.95,
                       num_similar = 3,
                       snr = 3,
                       mixed = TRUE,
                       cor_low = 0.7,
                       cor_up = 0.99,
                       randomseed = 7,
                       color_ratio = 0.9,
                       true_effect_size = 1,
                       size_ratio = 0.6,
                       none_ratio = 0.5,
                       is_legend = FALSE,
                       save = FALSE,
                       save_folder = "myexample"
                       ){
  if (mixed) {
    cors <- seq(cor_low,cor_up,length.out = trueL)
  } else {
    cors <- rep(cor_true, trueL)
  }
  
  
  set.seed(randomseed)
  effect_similar <- matrix(sample(1:p, trueL*(num_similar+1), replace = FALSE),
                           nrow = trueL)
  X <- matrix(rnorm(n * p), n, p)
  beta <- rep(0, p)
  beta[effect_similar[,1]] <- 1#rnorm(1)
  
  for (i in 1:trueL){
    for (j in 1:num_similar){
      X[,effect_similar[i,j+1]] <- (cors[i] * X[,effect_similar[i,1]] 
                                    + sqrt(1-cors[i]^2) * rnorm(n))
    }
  }
  
  y <- X %*% beta + sqrt(trueL/snr)*rnorm(n)
  
  
  output <- ibss(X,y,L)
  
  pip_est <- pip_cal(output)
  
  colors <- rainbow(trueL)
  lighten_colors <- adjustcolor(colors, alpha.f = color_ratio)
  
  point_colors <- rep("gray", length(pip_est))
  point_sizes <- rep(true_effect_size * none_ratio, length(pip_est))
  point_pchs <- rep(16, length(pip_est))
  
  
  if (save){
    
    if (!dir.exists(save_folder)) {
      dir.create(save_folder)
    }
    
    file_name <- paste(as.character(n),as.character(p), as.character(trueL),
                       as.character(snr),"plot.png", sep = "_")
    file_path <- file.path(save_folder, file_name)
    
    if (file.exists(file_path)) {
      file.remove(file_path)  # Remove the existing file if necessary
    }
    
    png(file_path, width = 2500, height = 1500, res = 300)
  }
  
  for (i in 1:trueL) {
    indices <- effect_similar[i, ]
    valid_indices <- indices[indices <= length(pip_est)]
    
    if (length(valid_indices) > 0) {
      point_colors[valid_indices] <- colors[i]
      point_sizes[valid_indices] <- true_effect_size
      point_sizes[valid_indices[-1]] <- true_effect_size * size_ratio
      point_colors[valid_indices[-1]] <- lighten_colors[i]
      point_pchs[valid_indices[1]] <- 19
    }
  }
  
  if(is_legend){
    layout(matrix(c(1, 2), nrow = 1, ncol = 2), widths = c(2, 1))
  }
  
  
  plot(1:length(pip_est), pip_est, 
       xlab = "variable", 
       ylab = "PIP", 
       main = "PIP Estimation and Credible Sets",
       pch = point_pchs,  
       col = point_colors,  
       cex = point_sizes)
  
  for (i in 1:length(point_colors)) {
    if (point_colors[i] != "gray") {
      points(i, pip_est[i], 
             pch = 19,  
             col = point_colors[i], 
             cex = point_sizes[i])  # Circle size
    }
  }
  
  
  c_s <- credible_sets(output, X)$result
  print(c_s)
  if (is.null(dim(c_s))){
    c_s <- c_s[c_s>0]
    c_s <- matrix(c_s, nrow=1)
  } else {
    c_s <- c_s[,c_s[1,]>0]
    if (is.null(dim(c_s))){
      c_s <- matrix(c_s, ncol=1)
    }
  }
  
  
  
  new_colors <- rainbow(ncol(c_s))  # Generate a new color array
  
  for (j in 1:ncol(c_s)) {
    if (max(c_s[, j]) > 0) {
      indices <- c_s[, j]
      valid_indices <- indices[indices > 0 & indices <= length(pip_est)]
      
      if (length(valid_indices) > 0) {
        points(valid_indices, pip_est[valid_indices], 
               pch = 1,  
               col = new_colors[j], 
               cex = true_effect_size * 1.5,  # Circle size
               lwd = true_effect_size * 2)  # Line width for thicker circles
      }
    }
  }
  
  if (is_legend) {
    plot.new() # Create a new plot
    par(xpd=TRUE)
    h_max = max(pip_est)
    dh = h_max /(trueL+2)
    dx = 0.05
    text(2*dx,h_max,paste("SNR:",as.character(snr)))
    text(2*dx,h_max-dh,"Simulated Correlation:")
    for ( i in 1:trueL){
      points(0, h_max-dh*i-dh,
             pch = 19,
             col = point_colors[effect_similar[i,1]],
             cex = point_sizes[effect_similar[i,1]])
      
      points(dx, h_max-dh*i-dh,
             pch = 19,
             col = point_colors[effect_similar[i,2]],
             cex = point_sizes[effect_similar[i,2]])
      
      text(6*dx, h_max-dh*i-dh, formatC(cors[i],format = "f",digits = 3))
    }
    par(xpd=FALSE)
    layout(1)
    
  }
  
  if (save){
    dev.off()
  }
}