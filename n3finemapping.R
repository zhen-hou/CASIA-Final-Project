for (simu in 1:2) {
  X <- N3finemapping$X
  y <- N3finemapping$Y[,simu]

  is_save = TRUE
  folder_name <-paste("plot", as.character(simu),sep="_")

  output <- ibss(X,y,L=10)

  true_effects <- which(N3finemapping$true_coef[,simu]!=0)
  pip_est <- pip_cal(output)

  pip_plot(pip_est, true_effects, credible_sets(output,X)$result, 
           save = is_save, save_folder = folder_name)


  pip_caviar <- as.vector(N3finemapping.CAVIAR[[simu]]$snp["snp_prob"])$snp_prob
  c_s_caviar <- N3finemapping.CAVIAR[[simu]]$set
  vari_caviar <- as.integer(as.vector(N3finemapping.CAVIAR[[simu]]$snp["snp"])$snp)
  pip_plot(pip_caviar, true_effects, c_s_caviar, vari_caviar, 
           method = "caviar", save = is_save, save_folder = folder_name)


  pip_dap <- N3finemapping.DAP[[simu]]$snp$snp_prob
  c_s_dap <- N3finemapping.DAP[[simu]]$set$snp
  vari_dap <- as.integer(N3finemapping.DAP[[simu]]$snp$snp)
  pip_plot(pip_dap, true_effects, c_s_dap, vari_dap, 
           method = "dap", save = is_save, save_folder = folder_name)


  pip_fm <- N3finemapping.FINEMAP[[simu]]$snp$snp_prob
  c_s_fm <- N3finemapping.FINEMAP[[simu]]$set$snp
  vari_fm <- as.integer(N3finemapping.FINEMAP[[simu]]$snp$snp)
  pip_plot(pip_fm, true_effects, c_s_fm, vari_fm, 
           method = "finemap", save = is_save, save_folder = folder_name)}
