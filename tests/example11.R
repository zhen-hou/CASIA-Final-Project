pip_est <- pip_cal(output)

color_ratio <- 0.9

true_effect_size <- 1
size_ratio <- 0.6
none_ratio <- 0.5

is_legend <- FALSE

colors <- rainbow(trueL)
lighten_colors <- adjustcolor(colors, alpha.f = color_ratio)

point_colors <- rep("gray", length(pip_est))
point_sizes <- rep(true_effect_size * none_ratio, length(pip_est))
point_pchs <- rep(16, length(pip_est))


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
if (is.null(dim(c_s))){
  c_s <- matrix(c_s, nrow=1)
}

new_colors <- rainbow(L)  # Generate a new color array

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
  dh = h_max /(trueL*3)
  dx = 0.05
  text(2*dx,h_max,"Simulated Correlation:")
  for ( i in 1:trueL){
    points(0, h_max-dh*i,
         pch = 19,
         col = point_colors[effect_similar[i,1]],
         cex = point_sizes[effect_similar[i,1]])
  
    points(dx, h_max-dh*i,
         pch = 19,
         col = point_colors[effect_similar[i,2]],
         cex = point_sizes[effect_similar[i,2]])
  
    text(3*dx, h_max-dh*i, cors[i])
  }
  par(xpd=FALSE)
  layout(1)

}


