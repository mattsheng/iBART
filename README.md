# iBART
Iterative BART: Variable and Operator Selection for feature engineering.

# Instruction
- `R` packages `bartMachine v1.2.6` and `glmnet v4.1-1` are required. 
- `bartMachine` requires `Java JDK`. Visit https://www.oracle.com/java/technologies/javase/jdk13-archive-downloads.html to download `Java JDK v13.0.2`.
- Please see https://github.com/kapelner/bartMachine for installation instruction for `bartMachine` if installation from R CRAN failed.

# Reproduce results in OIS paper
## Reproduce Section 3.2
- Make sure `operations.R` file from `/Sec_3.2_unary` is in your working directory.
- Run `x_complex_abs.R` to reproduce the simulation when `|x_1|` is the true descriptor, etc.
- 
## Reproduce Section 3.3
- Make sure `operations.R` file from `/Sec_3.3_binary` is in your working directory.
- Run `x_add.R` to reproduce the simulation when `x_1+x_2` is the true descriptor, etc.

## Reproduce Section 3.4
- Run `Sec_3.4_main.R` to reproduce results in Section 3.4

## Reproduce Section 4
- Run `Sec_4_full_data_main.R` to reproduce the results trained on full data.
- Run `Sec_4_out_sample_main.R` to reproduce the out-of-sample results.


# R Session Info
```
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252   LC_CTYPE=English_United States.1252    
[2] LC_MONETARY=English_United States.1252  LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  
[6] methods   base     

other attached packages:
 [1] glmnet_4.1-1        Matrix_1.3-4        bartMachine_1.2.6   missForest_1.4      
 [5] itertools_0.1-3     iterators_1.0.13    foreach_1.5.1      
 [8] randomForest_4.6-14 bartMachineJARs_1.1 rJava_1.0-4        

loaded via a namespace (and not attached):
[1] lattice_0.20-44  codetools_0.2-18 grid_4.0.5       splines_4.0.5    tools_4.0.5      
[6] survival_3.2-11  parallel_4.0.5   compiler_4.0.5   shape_1.4.6
```
