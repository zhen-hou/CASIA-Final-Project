# CASIA-Final-Project
This is a replication of paper "A simple new approach to variable selection in regression, with application to genetic fine mapping.", serving as part of final project for HKUST MATH 5472.

To run the code, first run `load_funs_datas.R` to load all the functions and datas needed. Reminder: need to change `folder_path` to path of folder `R_files`.

## Contents

`R_files` folders contains several R files. They implement the main function `ibss()` and other supporting functions.

`synthenic1.R` and `synthenic2.R` is the simulation on synthenic data. The main function for this part is `simulation()` defined in `R_files`.

`n3finemapping.R` is for simulation on data `N3finemapping.RData` which was downloaded at https://github.com/stephenslab/susieR/tree/master/data.
