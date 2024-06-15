#' @import glmnet
LASSO <- function(data = NULL, train_idx = NULL, type.measure = "deviance",
                  nfolds = 10, nlambda = 100, relax = TRUE,
                  gamma = c(0, 0.25, 0.5, 0.75, 1)) {
  # If there is only 1 selected variable, bypass LASSO step
  if (ncol(data$X) == 1) {
    return(data)
  }

  data$X <- as.matrix(data$X) # Make sure X is a matrix, not a vector
  if (!is.null(data$X_selected)) data$X_selected <- as.matrix(data$X_selected)
  # data$iBART_gen_size <- c(data$iBART_gen_size, ncol(data$X))
  dat <- trainingSplit(X = data$X, y = data$y, train_idx = train_idx)

  ######## LASSO ########
  cvfit <- cv.glmnet(x = dat$X_train, y = dat$y_train,
                     type.measure = type.measure,
                     nfolds = nfolds,
                     nlambda = nlambda,
                     relax = relax,
                     gamma = gamma)
  # In-sample
  yhat <- predict(cvfit, newx = dat$X_train, s = "lambda.min", gamma = if (relax) "gamma.min" else 1)
  data$iBART_in_sample_RMSE <- sqrt(mean((yhat - dat$y_train)^2))

  # Out-of-sample
  if (!is.null(train_idx)) {
    yhat <- predict(cvfit, newx = dat$X_test, s = "lambda.min", gamma = if (relax) "gamma.min" else 1)
    data$iBART_out_sample_RMSE <- sqrt(mean((yhat - dat$y_test)^2))
  }

  beta <- coef(cvfit, s = "lambda.min", gamma = if (relax) "gamma.min" else 1)
  pos_idx <- beta@i[-1] # remove intercept index

  # Check if LASSO selected any variable
  if (length(pos_idx) == 0) {
    message("LASSO did not select any variable...")
    message("Running least squares instead...")
    lm.data <- data.frame(y = dat$y_train, dat$X_train)
    data$iBART_model <- lm(y ~ ., data = lm.data)
    data$coefficients <- coef(data$iBART_model)
    names(data$coefficients) <- c("Intercept", data$descriptor_names)

    # In-sample
    yhat <- predict(data$iBART_model, newx = dat$X_train)
    data$iBART_in_sample_RMSE <- sqrt(mean((yhat - dat$y_train)^2))

    # Out-of-sample
    if (!is.null(train_idx)) {
      yhat <- predict(data$iBART_model, newx = dat$X_test)
      data$iBART_out_sample_RMSE <- sqrt(mean((yhat - dat$y_test)^2))
    }

    data$X_selected <- data$X
    data$name_selected <- data$name
    if (!is.null(data$unit)) data$unit_selected <- data$unit
    colnames(data$X_selected) <- colnames(data$X) <- data$name_selected # new
    data$iBART_sel_size <- c(data$iBART_sel_size, ncol(data$X_selected))
    data$descriptor_names <- data$name_selected

  } else {
    data$X_selected <- data$X <- as.matrix(data$X[, pos_idx]) # in case length(pos_idx) == 1
    data$name_selected <- data$name <- data$name[pos_idx]
    if (!is.null(data$unit)) data$unit_selected <- data$unit <- as.matrix(data$unit[, pos_idx])
    colnames(data$X_selected) <- colnames(data$X) <- data$name_selected # new
    data$iBART_sel_size <- c(data$iBART_sel_size, length(pos_idx))

    data$iBART_model <- cvfit
    data$descriptor_names <- data$name_selected
    intercept <- beta[1]
    beta <- beta[-1]
    beta <- beta[beta != 0]
    data$coefficients <- c(intercept, beta)
    names(data$coefficients) <- c("Intercept", data$descriptor_names)

  }

  # Return training and testing data
  if (is.null(train_idx)) {
    data$X_train <- data$X_selected
    data$X_test <- NULL
  } else {
    data$X_train <- dat$X_train
    data$X_test <- dat$X_test
  }

  return(data)
}
