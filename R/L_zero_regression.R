#' @import foreach

L_zero <- function(data = NULL, train_idx = NULL, standardize = TRUE, K = 5, parallel = FALSE, aic = FALSE, verbose = TRUE) {
  data$X <- as.matrix(data$X)
  dat <- trainingSplit(X = data$X, y = data$y, train_idx = train_idx)
  dat <- scaleData(data = dat, standardize = standardize)

  K <- min(K, ncol(dat$X_train))
  data$Lzero_models <- data$Lzero_names <- list()
  data$Lzero_in_sample_RMSE <- data$Lzero_out_sample_RMSE <- c()
  if (aic) aic_val <- c()

  if (verbose) cat("L-zero regression... \n")
  for (k in 1:K) {
    k_model <- k_var_model(dat$X_train, dat$y_train, dat$X_test, dat$y_test, k, parallel = parallel)
    data$Lzero_models[[k]] <- k_model$model
    data$Lzero_names[[k]] <- k_model$names
    data$Lzero_in_sample_RMSE[k] <- k_model$rmse_in
    data$Lzero_out_sample_RMSE[k] <- k_model$rmse_out
    if (aic) aic_val[k] <- AIC(k_model$model)
  }
  if (aic) {
    idx <- which.min(aic_val)
    data$Lzero_AIC_model <- data$Lzero_models[[idx]]
    data$Lzero_AIC_names <- data$Lzero_names[[idx]]
    data$Lzero_AIC_in_sample_RMSE <- data$Lzero_in_sample_RMSE[idx]
    data$Lzero_AIC_out_sample_RMSE <- data$Lzero_out_sample_RMSE[idx]
  }
  names(data$Lzero_models) <- names(data$Lzero_names) <- names(data$Lzero_in_sample_RMSE) <- names(data$Lzero_out_sample_RMSE) <-
    paste0(1:K, "-descriptor")
  return(data)
}


#' @export
#' @title Best subset selection for linear regression
#'
#' @param X_train The design matrix used during training.
#' @param y_train The response variable used during training.
#' @param X_test The design matrix used during testing. Default is \code{X_test = NULL} and full data will be used to train the best subset linear regression model.
#' @param y_test The response variable used during testing. Default is \code{y_test = NULL} and full data will be used to train the best subset linear regression model.
#' @param k The maximum number of predictors allowed in the model. For example, \code{k = 5} will produce the best model 5 predictors.
#' @param parallel Logical flag for parallelization. Default is \code{parallel = FALSE}.
#' @return A list of outputs.
#' \item{models}{An \code{lm} object storing the best k-predictor linear model.}
#' \item{names}{The variable name of the best k predictors.}
#' \item{rmse_in}{In-sample RMSE of the model.}
#' \item{rmse_out}{Out-of-sample RMSE of the model.}

k_var_model <- function(X_train, y_train, X_test = NULL, y_test = NULL, k = 1, parallel = FALSE) {
  p <- ncol(X_train)
  all_comb <- combn(1:p, k)
  N <- ncol(all_comb)

  if (parallel == TRUE) {
    rmse_in <- foreach(i = 1:N, .combine = "c") %dopar% {
      idx <- all_comb[, i]
      dat_train <- cbind(y_train, X_train[, idx])
      dat_train <- as.data.frame(dat_train)
      # colnames(dat_train) <- c("y_train", colnames(X_train)[idx]) # new

      # Build Least square model with k variables
      LS <- lm(y_train ~ ., data = dat_train)

      # Model selection via lowest RMSE
      rmse <- sqrt(mean(LS$residuals^2))
      rmse
    }
  } else {
    rmse_in <- rep(0, N)
    for (i in 1:N) {
      idx <- all_comb[, i]
      dat_train <- cbind(y_train, X_train[, idx])
      dat_train <- as.data.frame(dat_train)
      # colnames(dat_train) <- c("y_train", colnames(X_train)[idx]) # new

      # Build Least square model with k variables
      LS <- lm(y_train ~ ., data = dat_train)

      # Model selection via lowest RMSE
      rmse_in[i] <- sqrt(mean(LS$residuals^2))
    }
  }

  # index that have the lowest RMSE_in
  best_idx <- which.min(rmse_in)
  idx <- all_comb[, best_idx]
  names <- colnames(X_train)[idx]

  # rebuilt best model
  dat_train <- as.data.frame(cbind(y_train, X_train[, idx]))
  colnames(dat_train) <- c("y_train", names) # new
  best_model <- lm(y_train ~ ., data = dat_train)


  if (!is.null(X_test) && !is.null(y_test)) {
    # calculate RMSE_out
    dat_test <- cbind(rep(1, length(y_test)), X_test[, idx])
    dat_test <- as.matrix(dat_test)
    coef <- as.vector(best_model$coefficients)
    coef[is.na(coef)] <- 0
    yhat <- dat_test %*% coef
    rmse_out <- sqrt(mean((yhat - y_test)^2))
  }

  result <- list(models = best_model,
                 names = names,
                 rmse_in = min(rmse_in),
                 rmse_out = if (is.null(y_test)) NA else rmse_out)
  return(result)
}
