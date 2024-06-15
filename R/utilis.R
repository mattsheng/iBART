trainingSplit <- function(X, y, train_idx = NULL) {
  if (is.null(train_idx)) {
    return(list(X_train = as.matrix(X),
                X_test = NULL,
                y_train = y,
                y_test = NULL))
  }
  return(list(X_train = as.matrix(X[train_idx, ]),
              X_test = as.matrix(X[-train_idx, ]),
              y_train = y[train_idx],
              y_test = y[-train_idx]))
}

scaleData <- function(data, standardize = TRUE) {
  if (standardize) {
    train_col_mean <- colMeans(data$X_train, na.rm = TRUE)
    train_col_sd <- apply(data$X_train, 2, sd, na.rm = TRUE)
    data$X_train <- scale(data$X_train)
    data$X_test <- if (is.null(data$X_test)) NULL else sapply(1:ncol(data$X_test), function(k) (data$X_test[, k] - train_col_mean[k]) / train_col_sd[k])
  }
  return(data)
}

writeLogFunc <- function(data = NULL, K = 1, count = NULL, seed = NULL, out_sample = FALSE) {
  K <- min(length(data$name_selected), K)

  # Write outputs
  sink(file = paste0("output", count, ".txt"))
  cat(paste("seed:", seed, "\n"))
  cat(paste("iBART in-sample RMSE:", data$iBART_in_sample_RMSE, "\n"))
  if (out_sample) cat(paste("iBART out-sample RMSE:", data$iBART_out_sample_RMSE, "\n\n"))
  cat("Selected Descriptors: \n")
  cat(data$name_selected, sep = "\n")
  if (!is.null(data$Lzero_models)) {
    for (k in 1:K) {
      cat("---------------- \n")
      cat(paste0("No of descriptors: ", k, "\n"))
      cat("---------------- \n")
      cat(paste("RMSE in-sample:", data$Lzero_in_sample_RMSE[k], "\n"))
      if (out_sample) cat(paste("RMSE out-sample:", data$Lzero_out_sample_RMSE[k], "\n"))
      cat("Descriptors: \n")
      cat(data$Lzero_names[[k]], sep = "\n")
      cat("\n")
    }
  }
  cat(paste("Time:", round(data$runtime, digits = 2)))
  sink()
}


dataprocessing <- function(data) {
  data$X <- as.matrix(data$X)

  # Remove NA's
  na_idx <- apply(data$X, 2, function(x) any(is.na(x)))
  data$X <- as.matrix(data$X[, !na_idx])
  data$name <- data$name[!na_idx]
  if (!is.null(data$unit)) data$unit <- as.matrix(data$unit[, !na_idx])

  # Remove duplicated data
  dup_idx <- duplicated(data$X, MARGIN = 2)
  data$X <- as.matrix(data$X[, !dup_idx])
  data$name <- data$name[!dup_idx]
  if (!is.null(data$unit)) data$unit <- as.matrix(data$unit[, !dup_idx])

  # Remove columns with -Inf, Inf
  inf_idx <- apply(data$X, 2, function(x) any(abs(x) == Inf))
  data$X <- as.matrix(data$X[, !inf_idx])
  data$name <- data$name[!inf_idx]
  if (!is.null(data$unit)) data$unit <- as.matrix(data$unit[, !inf_idx])

  # Remove columns full of zero
  zero_idx <- apply(data$X, 2, function(x) all(x == 0))
  data$X <- as.matrix(data$X[, !zero_idx])
  data$name <- data$name[!zero_idx]
  if (!is.null(data$unit)) data$unit <- as.matrix(data$unit[, !zero_idx])
  colnames(data$X) <- data$name

  return(data)
}
