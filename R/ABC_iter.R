#' @import ABCforest
#' @importFrom utils capture.output
ABC_iter <- function(X, y, head, dimen,
                     X_selected = NULL,
                     head_selected = NULL,
                     dimen_selected = NULL,
                     nreps = 2000,
                     num_trees = 20,
                     num_burn_in = 1000,
                     num_iterations_after_burn_in = 2,
                     top = 200,
                     perm = FALSE,
                     standardize = TRUE,
                     train_idx = NULL,
                     seed = NULL,
                     parallel = FALSE,
                     iter = NULL) {
  X <- as.matrix(X)
  if (!is.null(X_selected)) X_selected <- as.matrix(X_selected)
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

  # Standardize data
  if (standardize) {
    X_train_scale <- as.matrix(scale(X_train))
  } else {
    X_train_scale <- as.matrix(X_train)
  }

  invisible(capture.output(
    abc_fit <- ABC_TREE(y_train, X_train_scale, nrep = nreps,
                        Ntrees = num_trees,
                        burnin = num_burn_in,
                        keep = num_iterations_after_burn_in,
                        parallel = parallel)))
  abc_ip <- process.abc(abc_fit, top = top, method = "top")
  names(abc_ip) <- colnames(X_train_scale)

  if (perm == FALSE) {
    pos_idx <- which(abc_ip >= 0.5)
  } else {
     var_sel <-
       abc_var_selection_by_permute(X = X_train_scale, y = y_train,
                                    num_reps_for_avg = 10,
                                    num_permute_samples = 50,
                                    nrep = nreps,
                                    Ntrees = num_trees,
                                    burnin = num_burn_in,
                                    top = top,
                                    keep = num_iterations_after_burn_in,
                                    parallel = parallel)
     pos_idx <- var_sel$se_pos_idx
  }

  # NA in pos_idx means no feature is selected
  if ((length(pos_idx) == 0) | (anyNA(pos_idx))) pos_idx <- NULL

  # Check if BART selected any variable
  if ((iter == 1) & (is.null(pos_idx))) {
    cat("iBART didn't select any features in the 1st iteration. Please consider increasing num_trees.")
    result <- list(X_selected = NULL,
                   head_selected = NULL,
                   dimen_selected = NULL,
                   pos_idx = pos_idx,
                   ABC_ip = abc_ip)
    return(result)
  } else {
    if (!is.null(X_selected)) {
      if (!is.null(pos_idx)) {
        X_selected <- cbind(X_selected, as.matrix(X[, pos_idx]))
        head_selected <- c(head_selected, head[pos_idx])
        if (is.null(dimen)) {
          dimen_selected <- NULL
        } else {
          dimen_selected <- c(dimen_selected, dimen[pos_idx])
        }
      }

      # Remove duplicated data
      temp <- round(X_selected, digits = 6)
      dup_index <- duplicated(temp, MARGIN = 2)
      X_selected <- X_selected[, !dup_index]
      head_selected <- head_selected[!dup_index]
      if (is.null(dimen)) {
        dimen_selected <- NULL
      } else {
        dimen_selected <- dimen_selected[!dup_index]
      }
    } else {
      X_selected <- as.matrix(X[, pos_idx])
      head_selected <- head[pos_idx]
      if (is.null(dimen)) {
        dimen_selected <- NULL
      } else {
        dimen_selected <- dimen[pos_idx]
      }
    }
    BART_output <- list(X_selected = X_selected,
                        head_selected = head_selected,
                        dimen_selected = dimen_selected,
                        pos_idx = pos_idx,
                        ABC_ip = abc_ip)
    return(BART_output)
  }
}
