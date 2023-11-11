#' @export
#' @importFrom stats coef lm predict sd cor AIC quantile
#' @importFrom utils combn
#' @title iBART descriptor selection
#'
#' @param X Input matrix of primary features \eqn{X}.
#' @param y Response variable \eqn{y}.
#' @param head Optional: name of primary features.
#' @param unit Optional: units and their respective dimensions of primary features. This is used to perform dimension analysis for generated descriptors to avoid generating unphyiscal descriptors, such as \eqn{size + size^2}. See \code{generate_dimension()} for details.
#' @param BART_var_sel_method Variable selection criterion used in BART. Three options are available: (1) "global_se", (2) "global_max", (3) "local". The default is "global_se". See \code{var_selection_by_permute} in \code{R} package \code{bartMachine} for more detail.
#' @param num_trees BART parameter: number of trees to be grown in the sum-of-trees model. If you want different values for each iteration of BART, input a vector of length equal to number of iterations. Default is \code{num_trees = 20}.
#' @param num_burn_in BART parameter: number of MCMC samples to be discarded as ``burn-in". If you want different values for each iteration of BART, input a vector of length equal to number of iterations. Default is \code{num_burn_in = 10000}.
#' @param num_iterations_after_burn_in BART parameter: number of MCMC samples to draw from the posterior distribution of \eqn{hat{f}(x)}. If you want different values for each iteration of BART, input a vector of length equal to number of iterations. Default is \code{num_iterations_after_burn_in = 5000}.
#' @param num_reps_for_avg BART parameter: number of replicates to over over to for the BART model's variable inclusion proportions. If you want different values for each iteration of BART, input a vector of length equal to number of iterations. Default is \code{num_reps_for_avg = 10}.
#' @param num_permute_samples BART parameter: number of permutations of the response to be made to generate the “null” permutation distribution. If you want different values for each iteration of BART, input a vector of length equal to number of iterations. Default is \code{num_permute_samples = 50}.
#' @param type.measure \code{glmnet} parameter: loss to use for cross-validation. The default is \code{type.measure="deviance"}, which uses squared-error for Gaussian models (a.k.a \code{type.measure="mse" there}). \code{type.measure="mae"} (mean absolute error) can be used also.
#' @param nfolds \code{glmnet} parameter: number of folds - default is 10. Smallest value allowable is \code{nfolds=3}.
#' @param nlambda \code{glmnet} parameter: the number of \code{lambda} values - default is 100.
#' @param relax \code{glmnet} parameter: If \code{TRUE}, then CV is done with respect to the mixing parameter \code{gamma} as well as \code{lambda}. Default is \code{relax=FALSE}.
#' @param gamma \code{glmnet} parameter: the values of the parameter for mixing the relaxed fit with the regularized fit, between 0 and 1; default is \code{gamma = c(0, 0.25, 0.5, 0.75, 1)}
#' @param opt A vector of operation order. For example, \code{opt = c("unary", "binary", "unary")} will apply unary operators, then binary operators, then unary operators. Available operator sets are \code{"unary"}, \code{"binary"}, and \code{"all"}, where \code{"all"} is the union of \code{"unary"} and \code{"binary"}.
#' @param sin_cos Logical flag for using \eqn{sin(\pi*x)} and \eqn{cos(\pi*x)} to generate descriptors. This is useful if you think there is periodic relationship between predictors and response. Default is \code{sin_cos = FALSE}.
#' @param apply_pos_opt_on_neg_x Logical flag for applying non-negative-valued operators, such as \eqn{\sqrt x} and \eqn{log(x)}, when some values of \eqn{x} is negative. If \code{apply_pos_opt_on_neg_x == TRUE}, apply absolute value operator first then non-negative-valued operator, i.e. generate \eqn{\sqrt |x|} and \eqn{log(|x|)} instead. Default is \code{apply_pos_opt_on_neg_x = TRUE}.
#' @param hold Number of iterations to hold. This allows iBART to run consecutive operator transformations before screening. Note \code{hold = 0} is equivalent to no skipping of variable selection in each iBART iterations. It should be less than \code{iter}.
#' @param pre_screen Logical flag for pre-screening the primary features X using BART. Only selected primary features will be used to generate descriptors. Note that \code{pre_screen = FALSE} is equivalent to \code{hold = 1}.
#' @param corr_screen Logical flag for screening out primary features that are independet of the response variable \eqn{y}.
#' @param out_sample Logical flag for out of sample assessment. Default is \code{out_sample = FALSE}.
#' @param train_idx Numerical vector storing the row indices for training data. Please set \code{out_sample = TRUE} if you supplied \code{train_idx}.
#' @param train_ratio Proportion of data used to train model. Value must be between (0,1]. This is only needed when \code{out_sample = TRUE} and \code{train_idx == NULL}. Default is \code{train_ratio = 1}.
#' @param Lzero Logical flag for L-zero variable selection. Default is \code{Lzero = TRUE}.
#' @param parallel Logical flag for parallel L-zero variable selection. Default is \code{parallel = FALSE}.
#' @param K If \code{Lzero == TRUE}, \code{K} sets the maximum number of descriptors to be selected.
#' @param aic If \code{Lzero == TRUE}, logical flag for selecting best number of descriptors using AIC. Possible number of descriptors are \eqn{1 \le k \le K}.
#' @param standardize Logical flag for data standardization prior to model fitting in BART and LASSO. Default is \code{standardize = TRUE}.
#' @param writeLog Logical flag for writing log file. The log file will contain information such as the descriptors selected by iBART, RMSE of the linear model build on the selected descriptors, etc. Default is \code{writeLog = FALSE}.
#' @param count Internal parameter. Default is \code{count = NULL}.
#' @param seed Optional: sets the seed in both R and Java. Default is \code{seed = NULL} which does not set the seed in R nor Java.
#' @return A list of iBART output.
#' \item{iBART_model}{The LASSO output of the last iteration of iBART. The predictors with non-zero coefficient are called the iBART selected descriptors.}
#' \item{X_selected}{The numerical values of the iBART selected descriptors.}
#' \item{descriptor_names}{The names of the iBART selected descriptors.}
#' \item{coefficients}{Coefficients of the iBART model. The first element is an intercept.}
#' \item{X_train}{The training matrix used in the last iteration.}
#' \item{X_test}{The testing matrix used in the last iteration.}
#' \item{iBART_gen_size}{The number of descriptors generated by iBART in each iteration.}
#' \item{iBART_sel_size}{The number of descriptors selected by iBART in each iteration.}
#' \item{iBART_in_sample_RMSE}{In sample RMSE of the LASSO model.}
#' \item{iBART_out_sample_RMSE}{Out of sample RMSE of the LASSO model if \code{out_sample == TRUE}.}
#' \item{Lzero_models}{The \eqn{l_0}-penalized regression models fitted on the iBART selected descriptors for \eqn{1 \le k \le K}.}
#' \item{Lzero_names}{The name of the best \eqn{k}D descriptors selected by the \eqn{l_0}-penalized regression model for \eqn{1 \le k \le K}.}
#' \item{Lzero_in_sample_RMSE}{In sample RMSE of the \eqn{l_0}-penalized regression model for \eqn{1 \le k \le K}.}
#' \item{Lzero_out_sample_RMSE}{Out of sample RMSE of the \eqn{l_0}-penalized regression model for \eqn{1 \le k \le K} if \code{out_sample == TRUE}.}
#' \item{Lzero_AIC_model}{The best \eqn{l_0}-penalized regression model selected by AIC.}
#' \item{Lzero_AIC_names}{The best \eqn{k}D descriptors where \eqn{1 \le k \le K} is chosen via AIC.}
#' \item{Lzero_AIC_in_sample_RMSE}{In sample RMSE of the best \eqn{l_0}-penalized regression models chosen by AIC.}
#' \item{Lzero_AIC_out_sample_RMSE}{Out of sample RMSE of the best \eqn{l_0}-penalized regression models chosen by AIC if \code{out_sample == TRUE}.}
#' \item{runtime}{Runtime in second.}
#'
#' @author
#' Shengbin Ye and Meng Li
#'
#' @references
#' Shengbin Ye, Meng Li (2021). Operator-induced structural variable selection with applications to materials genomes.
#' Submitted
#'
#' @examples
#' set.seed(123)
#' options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
#' library(iBART)
#' n <- 250
#' p <- 10
#'
#' X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
#' colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
#' y <- 15*(exp(X[,1])-exp(X[,2]))^2 + 20*sin(pi*X[,3]*X[,4])
#'        + rnorm(n, mean = 0, sd = 0.5)
#' \dontrun{
#' iBART_results <- iBART(X = X, y = y,
#'                        head = colnames(X),
#'                        unit = NULL,
#'                        opt = c("unary", "binary", "unary"), # unary operator first
#'                        sin_cos = TRUE,
#'                        apply_pos_opt_on_neg_x = FALSE,
#'                        Lzero = TRUE,
#'                        K = 4,
#'                        aic = TRUE,
#'                        standardize = FALSE,
#'                        seed = 99)
#' # Correct descriptor names are (exp(x.1)-exp(x.2))^2 and sin(pi*x.3*x.4)
#' iBART_results$descriptor_names
#' }


iBART <- function(X = NULL, y = NULL,
                  head = NULL,
                  unit = NULL,
                  BART_var_sel_method = "global_se",
                  num_trees = 20,
                  num_burn_in = 10000,
                  num_iterations_after_burn_in = 5000,
                  num_reps_for_avg = 10,
                  num_permute_samples = 50,
                  type.measure = "deviance",
                  nfolds = 10,
                  nlambda = 100,
                  relax = FALSE,
                  gamma = c(0, 0.25, 0.5, 0.75, 1),
                  opt = c("binary", "unary", "binary"),
                  sin_cos = FALSE,
                  apply_pos_opt_on_neg_x = TRUE,
                  hold = 0,
                  pre_screen = TRUE,
                  corr_screen = TRUE,
                  out_sample = FALSE,
                  train_idx = NULL,
                  train_ratio = 1,
                  Lzero = TRUE,
                  parallel = FALSE,
                  K = ifelse(Lzero, 5, 0),
                  aic = FALSE,
                  standardize = TRUE,
                  writeLog = FALSE,
                  count = NULL,
                  seed = NULL) {

  iter <- length(opt)
  nx <- nrow(X)
  px <- ncol(X)

  #### Check inputs ####
  if (iter < 1) stop("Length of opt must be at least 1.")

  if ((is.null(X)) || is.null(y)) stop("You need to give iBART a training set by specifying X and y \n")

  if (!is.matrix(X)) stop("The training data X must be a matrix", call. = FALSE)

  if (px == 0) stop("Your data matrix must have at least one predictor.")

  if (nx == 0) stop("Your data matrix must have at least one observation.")

  if (length(y) != nx) stop("The number of responses must be equal to the number of observations in the training data X.")

  if (!(BART_var_sel_method %in% c("global_se", "global_max", "local"))) {
    stop("BART_var_sel_method must be \"global_se\", \"global_max\", or \"local\"!!")
  }

  if (length(num_trees) == 1) {
    num_trees <- rep(num_trees, iter)
  } else if ((length(num_trees) > 1) && (length(num_trees) != iter)) {
    stop("Length of number of trees must equal to number of iterations!")
  }

  if (length(num_burn_in) == 1) {
    num_burn_in <- rep(num_burn_in, iter)
  } else if ((length(num_burn_in) > 1) && (length(num_burn_in) != iter)) {
    stop("Length of number of burn-in must equal to number of iterations!")
  }

  if (length(num_iterations_after_burn_in) == 1) {
    num_iterations_after_burn_in <- rep(num_iterations_after_burn_in, iter)
  } else if ((length(num_iterations_after_burn_in) > 1) && (length(num_iterations_after_burn_in) != iter)) {
    stop("Length of number of iteration after burn-in must equal to number of iterations!")
  }

  if (length(num_reps_for_avg) == 1) {
    num_reps_for_avg <- rep(num_reps_for_avg, iter)
  } else if ((length(num_reps_for_avg) > 1) && (length(num_reps_for_avg) != iter)) {
    stop("Length of number of replicates for average must equal to number of iterations!")
  }

  if (length(num_permute_samples) == 1) {
    num_permute_samples <- rep(num_permute_samples, iter)
  } else if ((length(num_permute_samples) > 1) && (length(num_permute_samples) != iter)) {
    stop("Length of number of permutations of the response must equal to number of iterations!")
  }

  if ((!is.numeric(hold)) || (hold > iter)) stop("hold must be an integer less than length of opt!")

  if (!is.logical(out_sample)) stop("out_sample must be a logical variable.")

  if ((!is.numeric(train_ratio)) || (train_ratio <= 0) || (train_ratio > 1)){
    stop("train_ratio must be a number between 0 and 1.")
  }

  if (!is.logical(Lzero)) stop("Lzero must be a logical variable.")

  if (!is.logical(writeLog)) stop("writeLog must be a logical variable.")

  if (!is.null(train_idx)) {
    if (!is.numeric(train_idx)) {
      stop("train_idx must be a numerical vector.")
    } else if (length(train_idx) > nrow(X)) {
      stop("The length of train_idx must be <= nrow(X).")
    }
  }

  # Get column names if primary feature names are not provided
  if (is.null(head)) {
    colnames(X) <- head <- paste("V", seq(from = 1, to = ncol(X), by = 1), sep = "")
  } else {
    colnames(X) <- head
  }

  if (!is.null(seed)) set.seed(seed)

  #### Generating training set ####
  start_time <- Sys.time()
  if (out_sample & is.null(train_idx)) {
    train_idx <- sample(1:nrow(X), floor(train_ratio * nrow(X)))
  }

  if (corr_screen) {
    # Calculate marginal correlation
    cor_mat <- suppressWarnings(abs(cor(x = X, y = y)))[, 1] # scalars will cause warning

    # Remove X cols that are independent of Y
    indep_idx <- is.na(cor_mat)
    X <- as.matrix(X[, !indep_idx])
    head <- head[!indep_idx]
    if (!is.null(unit)) unit <- unit[!indep_idx]
  }

  #### iBART descriptor generation and selection ####
  cat("Start iBART descriptor generation and selection... \n")
  dat <- list(y = y, X = X, head = head, unit = unit,
              X_selected = NULL, head_selected = NULL, dimen_selected = NULL,
              # pos_idx_old = NULL, pos_idx_new = NULL,
              iBART_gen_size = c(px), iBART_sel_size = c(),
              iBART_in_sample_RMSE = NULL,
              iBART_out_sample_RMSE = NULL,
              no_sel_count = 0,
              error = FALSE)

  #### Pre Screen ? ####
  if ((!pre_screen) & (hold == 0)) {
    hold <- 1
  }

  for (i in 1:iter) {
    cat(paste("Iteration", i, "\n", sep = " "))
    #### Hold operation ####
    if ((hold > 0) & (i <= hold)) {
      dat <- descriptorGenerator(dat, opt[i], sin_cos, apply_pos_opt_on_neg_x)
      dat$iBART_gen_size <- c(dat$iBART_gen_size, ncol(dat$X))
      dat$iBART_sel_size <- c(dat$iBART_sel_size, NA)
      cat("Skipping iBART descriptor selection... \n")
      next
    }
    cat("iBART descriptor selection... \n")

    ### BART-G.SE ###
    dat <- BART_iter(data = dat,
                     num_trees = num_trees[i],
                     num_burn_in = num_burn_in[i],
                     num_iterations_after_burn_in = num_iterations_after_burn_in[i],
                     num_reps_for_avg = num_reps_for_avg[i],
                     num_permute_samples = num_permute_samples[i],
                     standardize = standardize,
                     train_idx = train_idx,
                     seed = seed,
                     iter = i)

    # If iBART didn't select anything in 2 consecutive iterations
    if (dat$no_sel_count > 1) {
      message("BART didn't select anything in 2 consecutive iterations.")
      break
    }

    ### Feature engineering via operations ###
    dat <- descriptorGenerator(dat, opt[i], sin_cos, apply_pos_opt_on_neg_x)
    dat$iBART_gen_size <- c(dat$iBART_gen_size, ncol(dat$X))
    if (dat$error) break
  }

  cat("BART iteration done! \n")

  #### LASSO variable selection ####
  cat("LASSO descriptor selection... \n")
  dat <- LASSO(data = dat,
               train_idx = train_idx,
               type.measure = type.measure,
               nfolds = nfolds,
               nlambda = nlambda,
               relax = relax,
               gamma = gamma)

  #### L-zero regression ####
  if (Lzero) {
    dat <- L_zero(data = dat, train_idx = train_idx, standardize = standardize,
                  K = K, parallel = parallel, aic = aic)
  }

  end_time <- Sys.time()
  dat$runtime <- as.numeric(end_time - start_time, units = "secs")
  cat(paste("Total time:", dat$runtime, "secs \n", sep = " "))

  #### Generate output log to a .txt file ####
  if (writeLog) writeLogFunc(data = dat,
                             K = K,
                             count = count,
                             seed = seed,
                             out_sample = out_sample)

  # Report relevant stuff
  dat <- dat[c("iBART_model", "X_selected", "descriptor_names", "coefficients",
               "X_train", "X_test",
               "iBART_gen_size", "iBART_sel_size",
               "iBART_in_sample_RMSE", "iBART_out_sample_RMSE",
               "Lzero_models", "Lzero_names",
               "Lzero_in_sample_RMSE", "Lzero_out_sample_RMSE",
               "Lzero_AIC_model", "Lzero_AIC_names",
               "Lzero_AIC_in_sample_RMSE", "Lzero_AIC_out_sample_RMSE",
               "runtime")]

  # Add name to improve clarity
  names(dat$iBART_gen_size) <- paste0("Iteration_", 0:(length(dat$iBART_gen_size) - 1))
  names(dat$iBART_sel_size) <- paste0("Iteration_", 0:(length(dat$iBART_sel_size) - 1))

  # Remove NULL elements
  dat <- dat[-which(sapply(dat, is.null))]
  return(dat)
}
