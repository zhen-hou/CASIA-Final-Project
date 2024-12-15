# CASIA-Final-Project
This is a replication of paper "A simple new approach to variable selection in regression, with application to genetic fine mapping.", serving as part of final project for HKUST MATH 5472.

## Run the code

To run the code, first run `load_funs_datas.R` to load all the functions and datas needed. Reminder: need to change `folder_path` to path of folder `R_files`.

Then, you can run function `ibss(X,y,L)` to some n by p matrix `X` and n vector `y` and integer `L`. There are some other parameters for this function, which can be found in `ibss.R`.

You can also directly source simulation files such as `synthenic1.R` to see some results.

## Contents

`R_files` folders contains several R files. They implement the main function `ibss()` and other supporting functions.

`synthenic1.R` and `synthenic2.R` is the simulation on synthenic data. The main function for this part is `simulation()` defined in `R_files`.

`n3finemapping.R` is for simulation on data `N3finemapping.RData` which was downloaded at https://github.com/stephenslab/susieR/tree/master/data. `N3finemapping.CAVIAR.RData`, `N3finemapping.DAP.RData` and `N3finemapping.FINEMAP.RData` are results for corresponding model. They are downloaded at https://github.com/stephenslab/susieR/tree/master/inst/datafiles.

The other folders are for debug use and storing plot results.

## Reference

G. Wang, G., Sarkar, A., Carbonetto, P. \& Stephens, M. (2020). A simple new approach to variable selection in regression, with application to genetic fine mapping. Journal of the Royal Statistical Society, Series B 82, 1273–1300. https://doi.org/10.1111/rssb.12388

Mitchell, T. J. and Beauchamp, J. J. (1988) Bayesian variable selection in linear regression. J. Am. Statist. Ass., 83, 1023–1032.

Wen, X., Lee, Y., Luca, F. and Pique-Regi, R. (2016) Efficient integrative multi-SNP association analysis via deterministic approximation of posteriors. Am. J. Hum. Genet., 98, 1114–1129.

Benner, C., Spencer, C. C., Havulinna, A. S., Salomaa, V., Ripatti, S. and Pirinen, M. (2016) FINEMAP: efficient variable selection using summary data from genome-wide association studies. Bioinformatics, 32, 1493–1501.
