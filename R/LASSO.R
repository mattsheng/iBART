LASSO <- function(X, y, head, dimen, train_idx = NULL) {
  if (is.null(train_idx)) {
    X_train <- X
    y_train <- y
  } else {
    # Training data
    X_train <- X[train_idx, ]
    y_train <- y[train_idx]

    # Testing data
    X_test <- X[-train_idx, ]
    y_test <- y[-train_idx]
  }

  ######## LASSO ########
  cvfit <- cv.glmnet(x = as.matrix(X_train), y = y_train, nfolds = 10)
  # In-sample
  yhat <- predict(cvfit, newx = as.matrix(X_train), s = "lambda.min")
  RMSE_LASSO_in_sample <- sqrt(mean((yhat - y_train)^2))

  # Out-of-sample
  if (!is.null(train_idx)) {
    yhat <- predict(cvfit, newx = as.matrix(X_test), s = "lambda.min")
    RMSE_LASSO_out_sample <- sqrt(mean((yhat - y_test)^2))
  }

  beta <- coef(cvfit, s = "lambda.min")
  pos_idx <- beta@i[-1] # remove intercept index
  # pos_idx <- pos_idx[-1]

  # Check if LASSO selected any variable
  if (length(pos_idx) == 0) {
    stop("LASSO did not select any variable, trying another seed...")
  } else {
    X_selected <- X[, pos_idx]
    head_selected <- head[pos_idx]
    if (is.null(dimen)) {
      dimen_selected <- NULL
    } else {
      dimen_selected <- dimen[pos_idx]
    }

    LASSO_output <- list(X_selected = X_selected,
                         head_selected = head_selected,
                         dimen_selected = dimen_selected,
                         In_sample_RMSE = RMSE_LASSO_in_sample,
                         Out_sample_RMSE = ifelse(!is.null(train_idx), RMSE_LASSO_out_sample, NA),
                         LASSO_model = cvfit)
    return(LASSO_output)
  }
}
