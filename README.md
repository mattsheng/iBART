# iBART
An R package for iterative BART for Variable and Operator Selection with Operator Induced Structure (OIS).

Copyright (C) Shengbin Ye and Meng Li

Department of Statistics, Rice Univerisity.

# Setup Instructions
## Install Dependencies
- `R` packages `bartMachine v1.2.6` and `glmnet v4.1-1` are required for installing the `iBART` package in `R`.
- `bartMachine` requires `Java JDK` to be installed in your computer. Visit https://www.oracle.com/java/technologies/javase/jdk13-archive-downloads.html to download `Java JDK v13.0.2`.
- Use `install.packages("rJava")` within `R` after installing `Java JDK v13.0.2`.
- Please see https://github.com/kapelner/bartMachine for installation instruction for `rJava` and `bartMachine` if installation from R CRAN failed.
- Finally install `devtools` package using `install.packages("devtools")` in `R`. The `install_github()` function in `devtools` is required for installing `R` packages via GitHub.

## Install iBART via GitHub 
Use the following code to install `iBART` from GitHub:
```
library(devtools)
install_github("mattsheng/iBART")
```

# Reproduce results in OIS paper
## Reproduce Section 3.4
- Run `demo("Sec_3.4_main")` to reproduce results in Section 3.4


# R Session Info
```
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets 
[6] methods   base     

other attached packages:
 [1] iBART_0.0.1.5       glmnet_4.1-1       
 [3] Matrix_1.3-4        bartMachine_1.2.6  
 [5] missForest_1.4      itertools_0.1-3    
 [7] iterators_1.0.13    foreach_1.5.1      
 [9] randomForest_4.6-14 bartMachineJARs_1.1
[11] rJava_1.0-4        

loaded via a namespace (and not attached):
[1] lattice_0.20-44  codetools_0.2-18 grid_4.0.5      
[4] splines_4.0.5    tools_4.0.5      survival_3.2-11 
[7] parallel_4.0.5   compiler_4.0.5   shape_1.4.6 
```
