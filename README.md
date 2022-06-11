# iBART
An R package for iterative BART for Variable and Operator Selection with Operator Induced Structure (OIS).

Copyright (C) Shengbin Ye and Meng Li

Department of Statistics, Rice Univerisity.

# Setup Instructions
## Install Dependencies
- R packages `bartMachine >= v1.2.6` and `glmnet >= v4.1-1` are required for installing the `iBART` package in R.
- `bartMachine` requires `Java JDK` to be installed on your computer. Visit https://www.oracle.com/java/technologies/javase/jdk13-archive-downloads.html to download `Java JDK v13.0.2`.
- Install rJava using `install.packages("rJava")` within `R` after installing `Java JDK v13.0.2`.
- Install bartMachine using `install.packages("bartMachine", INSTALL_opts = "--no-multiarch").
- Install devtools using `install.packages("devtools")`. The `R` package devtools is required to install R packages from GitHub. 

## Install iBART via GitHub 
Use the following code to install `iBART` from GitHub:
```
library(devtools)
install_github("mattsheng/iBART", ref = "master")
```

# Reproduce results in OIS paper
## Reproduce Section 3.4
- Run `demo("Sec_3.4_main")` to reproduce results in Section 3.4


# R Session Info
```
R version 4.1.1 (2021-08-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] iBART_0.0.2.0       glmnet_4.1-2        Matrix_1.3-4       
 [4] bartMachine_1.2.6   missForest_1.4      itertools_0.1-3    
 [7] iterators_1.0.13    foreach_1.5.1       randomForest_4.6-14
[10] bartMachineJARs_1.1 rJava_1.0-5        

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.7       knitr_1.36       magrittr_2.0.1   splines_4.1.1   
 [5] lattice_0.20-45  rlang_0.4.12     fastmap_1.1.0    stringr_1.4.0   
 [9] plyr_1.8.6       tools_4.1.1      parallel_4.1.1   grid_4.1.1      
[13] xfun_0.27        htmltools_0.5.2  survival_3.2-13  yaml_2.2.1      
[17] digest_0.6.28    reshape2_1.4.4   codetools_0.2-18 shape_1.4.6     
[21] evaluate_0.14    rmarkdown_2.11   stringi_1.7.5    compiler_4.1.1
```

# Citation
If you use the iBART R package, please cite the following:
```
@misc{ye2021operatorinduced,
      title={Operator-induced structural variable selection with applications to materials genomes}, 
      author={Shengbin Ye and Thomas P. Senftle and Meng Li},
      year={2021},
      eprint={2110.10195},
      archivePrefix={arXiv},
      primaryClass={stat.ME}
}
```
