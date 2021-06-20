set.seed(1234) # seed = 123 for p = 10, 20, 50, 100
               # seed = 1234 for p = 200
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(iBART)

TruePositive <- function(names) {
  TP <- 0
  if (any(names == "(exp(x.1)-exp(x.2))^2")) {
    TP <- TP + 1
  }
  if (any(names == "sin(pi*(x.3*x.4))")) {
    TP <- TP + 1
  }
  return(TP)
}

FalsePositive <- function(names, TP) {
  FP <- length(names) - TP
  return(FP)
}

FalseNegative <- function(TP) {
  FN <- 2 - TP
  return(FN)
}

Precision <- function(TP, FP) {
  return(TP / (TP + FP))
}

Recall <- function(TP, FN) {
  return(TP / (TP + FN))
}

F1 <- function(precision, recall) {
  if (precision + recall == 0) {
    return(0)
  } else{
    return(2 * precision * recall / (precision + recall))
  }
}

seeds <- sample.int(1000, size = 50)
BART_names <- BART_aic_names <- list()
BART_rmse <- BART_aic_rmse <- rep(0, 50)
BART_TP <- BART_FP <- BART_FN <- BART_precision <- BART_recall <- BART_F1 <- rep(0, 50)
BART_aic_TP <- BART_aic_FP <- BART_aic_FN <- BART_aic_precision <- BART_aic_recall <- BART_aic_F1 <- rep(0, 50)
BART_gen_size <- BART_sel_size <- matrix(0, nrow = 50, ncol = 4)

n <- 250
p <- 10 # change p here to reproduce results in Section 3.5

for (j in 1:50) {
  set.seed(seeds[j])
  ################################ Generate data ################################
  X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
  y <- 15 * (exp(X[, 1]) - exp(X[, 2]))^2 + 20 * sin(pi * X[, 3] * X[, 4]) + rnorm(n, mean = 0, sd = 0.5)

  iBART_results <- iBART(X = X, y = y,
                         head = colnames(X),
                         dimen = NULL,
                         opt = 1, # unary operator first
                         sin_cos = TRUE,
                         apply_pos_opt_on_neg_x = FALSE,
                         iter = 3,
                         Lzero = TRUE,
                         K = 4,
                         AIC = TRUE,
                         standardize = FALSE,
                         writeLog = FALSE,
                         count = j,
                         seed = 99)
  BART_gen_size[j, ] <- iBART_results$iBART_gen_size
  BART_sel_size[j, ] <- iBART_results$iBART_sel_size

  # original model
  BART_TP[j] <- TruePositive(iBART_results$descriptor_names)
  BART_FP[j] <- FalsePositive(iBART_results$descriptor_names, BART_TP[j])
  BART_FN[j] <- FalseNegative(BART_TP[j])
  BART_precision[j] <- Precision(BART_TP[j], BART_FP[j])
  BART_recall[j] <- Recall(BART_TP[j], BART_FN[j])
  BART_F1[j] <- F1(BART_precision[j], BART_recall[j])

  # AIC model
  BART_aic_TP[j] <- TruePositive(iBART_results$Lzero_aic_names)
  BART_aic_FP[j] <- FalsePositive(iBART_results$Lzero_aic_names, BART_aic_TP[j])
  BART_aic_FN[j] <- FalseNegative(BART_aic_TP[j])
  BART_aic_precision[j] <- Precision(BART_aic_TP[j], BART_aic_FP[j])
  BART_aic_recall[j] <- Recall(BART_aic_TP[j], BART_aic_FN[j])
  BART_aic_F1[j] <- F1(BART_aic_precision[j], BART_aic_recall[j])

  cat("Iteration: ", j, "/50... \n", sep = "")
}
boxplot(BART_F1, BART_aic_F1) # F1 boxplot

#### Uncomment below to produce iBART selected and generated
#### sizes plot
# library(ggplot2)
# library(dplyr)
# iteration <- rep(rep(0:3, each = 50), 2)
# Type <- rep(c("Selected", "Generated"), each = 4*50)
# size <- c(as.vector(BART_sel_size), as.vector(BART_gen_size))
# iBART_data <- data.frame(size, Type, iteration)
# attach(iBART_data)
# data_by_iter <- iBART_data %>%
#   group_by(Type, iteration) %>%
#   summarise(Mean = mean(size))
# sd <- iBART_data %>%
#   group_by(Type, iteration) %>%
#   summarise(SD = sd(size)/sqrt(50))
# ggplot(data_by_iter, aes(x = iteration, y = Mean,
#                                  colour = Type, group = Type)) +
#   theme(text = element_text(size = 15)) +
#   geom_line(size = 1) +
#   geom_point(size = 3, shape = 21, fill = "white") +
#   geom_text(data = data_by_iter, aes(label = Mean, y = Mean + 5, group = Type),
#             position = position_dodge(0), size = 5, colour = "blue") +
#   labs(x = "Iteration", y = "Number of descriptors") +
#   scale_x_continuous(breaks = c(0, 1, 2, 3))
