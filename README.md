<!-- badges: start -->
[![R-CMD-check](https://github.com/mattsheng/iBART/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mattsheng/iBART/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# iBART

This is a R-Java implementation of iBART found in Ye, Senftle, & Li [Operator-induced structural variable selection for identifying materials genes](https://www.tandfonline.com/doi/abs/10.1080/01621459.2023.2294527). This R package depends on the R package [`bartMachine`](https://github.com/kapelner/bartMachine) for its BART-G.SE variable selection implementation.

## Installation

Before installing the iBART package in R, you first need to install Java JDK and rJava R package. 

### Install Java JDK (not JRE)

Download [Java 17 JDK or above](https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html) and install it properly. Then run `R CMD javareconf` from the command line to configure Java in R. iBART requires bartMachine and rJava which require Java JDK; Java JRE won't work!

### Install rJava

Run `install.packages("rJava", INSTALL_opts = "--no-multriarch")` within R. To reproduce results in the paper, please install `rJava 1.0-4`.

### Install bartMachine

Run `install.packages("bartMachine", INSTALL_opts = "--no-multiarch")` within R. To reproduce results in the paper, please install `bartMachineJARs 1.1` and `bartMachine 1.2.6`. If you experience error, please see the [bartMachine repo](https://github.com/kapelner/bartMachine) for detailed instructions.


### Install glmnet

Run `install.packages("glmnet")` within R. To reproduce results in the paper, please install `glmnet 4.1-1`.

### Install iBART via CRAN

Run `install.packages("iBART")` within R.

### Install iBART via devtools

Run `devtools::install_github("mattsheng/iBART", INSTALL_opts = "--no-multriarch", build_vignettes = TRUE)` within R.


## Example

We use the simulation model in Section 3.4 of [our paper](https://arxiv.org/abs/2110.10195) to demonstrate the usage of iBART. Vignettes for real data application and simulation are available at [here](https://github.com/mattsheng/iBART/tree/main/vignettes)

```
set.seed(123)
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(iBART)

n <- 250
p <- 10
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
y <- 15*(exp(X[,1])-exp(X[,2]))^2 + 20*sin(pi*X[,3]*X[,4])
       + rnorm(n, mean = 0, sd = 0.5)

iBART_results <- iBART(X = X, y = y,
                       name = colnames(X),
                       unit = NULL,                         # no unit information for simulation
                       opt = c("unary", "binary", "unary"), # unary operator first
                       sin_cos = TRUE,                      # add sin and cos to operator set
                       apply_pos_opt_on_neg_x = FALSE,      # e.g. do not apply log() on negative x
                       Lzero = TRUE,                        # best subset selection
                       K = 4,                               # at most 4 predictors in best subset model
                       standardize = FALSE,                 # don't standardize input matrix X
                       seed = 99)

# > Start iBART descriptor generation and selection... 
# > Iteration 1 
# > iBART descriptor selection... 
# > avg..........null..................................................
# > Constructing descriptors using unary operators... 
# > Iteration 2 
# > iBART descriptor selection... 
# > avg..........null..................................................
# > Constructing descriptors using binary operators... 
# > Iteration 3 
# > iBART descriptor selection... 
# > avg..........null..................................................
# > Constructing descriptors using unary operators... 
# > BART iteration done! 
# > LASSO descriptor selection... 
# > L-zero regression... 
# > Total time: 261.336249113083 secs

# Correct descriptor names are (exp(x.1)-exp(x.2))^2 and sin(pi*x.3*x.4)
iBART_results$descriptor_names
# > [1] "(exp(x.1)-exp(x.2))^2" "sin(pi*(x.3*x.4))"
```


## R Session Info
```
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] glmnet_4.1-1        Matrix_1.3-4        bartMachine_1.2.6  
 [4] missForest_1.4      itertools_0.1-3     iterators_1.0.13   
 [7] foreach_1.5.1       randomForest_4.6-14 bartMachineJARs_1.1
[10] rJava_1.0-4        

loaded via a namespace (and not attached):
[1] lattice_0.20-44  codetools_0.2-18 grid_4.0.5       splines_4.0.5   
[5] tools_4.0.5      survival_3.2-11  parallel_4.0.5   compiler_4.0.5  
[9] shape_1.4.6     
```
