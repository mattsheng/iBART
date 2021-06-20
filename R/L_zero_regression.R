#' @import foreach
k_var_model <- function(X_train, y_train, X_test = NULL, y_test = NULL, k, parallel) {
  p <- ncol(X_train)
  all_comb <- combn(1:p, k)
  N <- ncol(all_comb)

  if (parallel == TRUE) {
    rmse_in <- foreach(i = 1:N, .combine = "c") %dopar% {
      idx <- all_comb[, i]
      dat_train <- cbind(y_train, X_train[, idx])
      dat_train <- as.data.frame(dat_train)

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

      # Build Least square model with k variables
      LS <- lm(y_train ~ ., data = dat_train)

      # Model selection via lowest RMSE
      rmse_in[i] <- sqrt(mean(LS$residuals^2))
    }
  }

  # index that have the lowest RMSE_in
  best_idx <- which.min(rmse_in)
  idx <- all_comb[, best_idx]

  # rebuilt best model
  dat_train <- as.data.frame(cbind(y_train, X_train[, idx]))
  best_model <- lm(y_train ~ ., data = dat_train)
  names <- colnames(X_train)[idx]

  if (!is.null(X_test) && !is.null(y_test)) {
    # calculate RMSE_out
    dat_test <- cbind(rep(1, length(y_test)), X_test[, idx])
    dat_test <- as.matrix(dat_test)
    coef <- as.vector(best_model$coefficients)
    coef[is.na(coef)] <- 0
    yhat <- dat_test %*% coef

    rmse_out <- sqrt(mean((yhat - y_test)^2))
    rmse_in <- min(rmse_in)

    result <- list(rmse_out = rmse_out,
                   rmse_in = rmse_in,
                   models = best_model,
                   names = names
    )
  } else {
    rmse_in <- min(rmse_in)
    result <- list(rmse_out = NA,
                   rmse_in = rmse_in,
                   models = best_model,
                   names = names
    )
  }
  return(result)
}
