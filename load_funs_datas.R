folder_path <- "~/R_scripts/CASIfinal/R_files"
r_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file)
}

load("N3finemapping.RData")

load("N3finemapping.CAVIAR.RData")
load("N3finemapping.DAP.RData")
load("N3finemapping.FINEMAP.RData")