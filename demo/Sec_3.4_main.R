set.seed(123)
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(iBART)

n <- 250
p <- 10
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
y <- 15*(exp(X[,1])-exp(X[,2]))^2 + 20*sin(pi*X[,3]*X[,4]) + rnorm(n, mean = 0, sd = 0.5)

iBART_results <- iBART(X = X, y = y,
                       head = colnames(X),
                       unit = NULL,                         # no unit information for simulation
                       opt = c("unary", "binary", "unary"), # unary operator first
                       sin_cos = TRUE,                      # add sin and cos to operator set
                       apply_pos_opt_on_neg_x = FALSE,      # e.g. do not apply log() on negative x
                       Lzero = TRUE,                        # best subset selection
                       K = 4,                               # at most 4 predictors in best subset model
                       standardize = FALSE,                 # don't standardize input matrix X
                       seed = 99)

# Correct descriptor names are (exp(x.1)-exp(x.2))^2 and sin(pi*x.3*x.4)
iBART_results$descriptor_names
