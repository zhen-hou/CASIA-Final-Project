pip_plot <- function(pip_est,
                     true_effects,
                     c_s_est,
                     variables = 1:length(pip_est),
                     method = "ibss",
                     randomseed = 7,
                     color_ratio = 0.9,
                     true_effect_size = 1,
                     none_ratio = 0.5,
                     save = FALSE,
                     save_folder = "plots"
                     ){
  
  trueL <- length(true_effects)
  colors <- rainbow(trueL)
  
  point_colors <- rep("gray", length(pip_est))
  point_sizes <- rep(true_effect_size * none_ratio, length(pip_est))
  point_pchs <- rep(16, length(pip_est))
  
  
  for (i in 1:trueL) {
    point_colors[variables==true_effects[i]] <- colors[i]
    point_sizes[variables==true_effects[i]] <- true_effect_size
  }
  
  if (save){
    
    if (!dir.exists(save_folder)) {
      dir.create(save_folder)
    }
    
    file_name <- paste(method, "plot.png", sep = "_")
    file_path <- file.path(save_folder, file_name)
    
    if (file.exists(file_path)) {
      file.remove(file_path)  # Remove the existing file if necessary
    }
    
    png(file_path, width = 2000, height = 1500, res = 300)
  }
  
  plot(variables, pip_est, 
       xlab = "variable", 
       ylab = "PIP", 
       main = paste(toupper(method)),
       pch = point_pchs,  
       col = point_colors,  
       cex = point_sizes)
  
  
  
  if (method == "ibss"){
    if (is.null(dim(c_s_est))){
      c_s_est <- matrix(c_s_est, nrow=1)
    }
    
    c_s_est <- c_s_est[,c_s_est[1,]!=0]
    
    L <- ncol(c_s_est)
    
    new_colors <- rainbow(L)  # Generate a new color array
    
    for (j in 1:L) {
      if (max(c_s_est[, j]) > 0) {
        indices <- c_s_est[, j]
        valid_indices <- indices[indices > 0]
        
        if (length(valid_indices) > 0) {
          points(valid_indices, pip_est[valid_indices], 
                 pch = 1,  
                 col = new_colors[j], 
                 cex = true_effect_size * 1.5,  # Circle size
                 lwd = true_effect_size * 2)  # Line width for thicker circles
        }
      }
    }
  } else if (method == "caviar") {
    num_est <- length(c_s_est)
    new_colors <- rainbow(num_est)
    points(variables[1:num_est], pip_est[1:num_est],
           pch = 1,  
           col = new_colors[1], 
           cex = true_effect_size * 1.5,  # Circle size
           lwd = true_effect_size * 2)
  } else if (method == "dap") {
    num_est <- length(c_s_est)
    new_colors <- rainbow(num_est)
    for (k in 1:num_est) {
      set_contain <- as.numeric(strsplit(c_s_est[k], ",")[[1]])
      #print(pip_est[variables %in% set_contain])
      points(variables[variables %in% set_contain], 
             pip_est[variables %in% set_contain],
             pch = 1,  
             col = new_colors[k], 
             cex = true_effect_size * 1.5,  # Circle size
             lwd = true_effect_size * 2)
    }
  }
  
  for (i in 1:length(point_colors)) {
    if (point_colors[i] != "gray") {
      points(variables[i], pip_est[i], 
             pch = 19,  
             col = point_colors[i], 
             cex = point_sizes[i])  # Circle size
    }
  }
  
  if (save){
    dev.off()
  }
  
}