BART_iter <- function(X, y, head, dimen,
                      X_selected = NULL,
                      head_selected = NULL,
                      dimen_selected = NULL,
                      num_trees = 20,
                      num_burn_in = 10000,
                      num_iterations_after_burn_in = 5000,
                      num_reps_for_avg = 10,
                      num_permute_samples = 50,
                      standardize = TRUE,
                      train_idx = NULL,
                      seed = NULL) {
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

  # bartMachine only takes dataframe
  if (standardize == TRUE){
    X_train_scale <- as.data.frame(scale(X_train))
  } else {
    X_train_scale <- as.data.frame(X_train)
  }

  if(is.null(seed)){
    bart_machine <- bartMachine(X_train_scale, y_train,
                                num_trees = num_trees,
                                num_burn_in = num_burn_in,
                                num_iterations_after_burn_in = num_iterations_after_burn_in,
                                run_in_sample = FALSE,
                                serialize = FALSE,
                                verbose = FALSE)
  } else{
    bart_machine <- bartMachine(X_train_scale, y_train,
                                num_trees = num_trees,
                                num_burn_in = num_burn_in,
                                num_iterations_after_burn_in = num_iterations_after_burn_in,
                                run_in_sample = FALSE,
                                serialize = FALSE,
                                seed = seed,
                                verbose = FALSE)
  }

  var_sel <- var_selection_by_permute(bart_machine,
                                      num_reps_for_avg = num_reps_for_avg,
                                      num_permute_samples = num_permute_samples,
                                      num_trees_for_permute = 20,
                                      plot = FALSE)

  pos_idx <- sort(var_sel$important_vars_global_se_col_nums)

  # Check if BART selected any variable
  if (is.null(X_selected) && length(pos_idx) == 0) {
    stop("BART did not select any variable, trying another seed...")
  } else {
    if (!is.null(X_selected)) {
      X_selected <- cbind(X_selected, X[, pos_idx])
      head_selected <- c(head_selected, head[pos_idx])
      if (is.null(dimen)) {
        dimen_selected <- NULL
      } else {
        dimen_selected <- c(dimen_selected, dimen[pos_idx])
      }

      # Remove duplicated data
      temp <- round(X_selected, digits = 6)
      dup_index <- duplicated(temp, MARGIN = 2)
      if (any(dup_index == TRUE)) {
        X_selected <- X_selected[, !dup_index]
        head_selected <- head_selected[!dup_index]
        if (is.null(dimen)) {
          dimen_selected <- NULL
        } else {
          dimen_selected <- dimen_selected[!dup_index]
        }
      }
    } else {
      X_selected <- X[, pos_idx]
      head_selected <- head[pos_idx]
      if (is.null(dimen)) {
        dimen_selected <- NULL
      } else {
        dimen_selected <- dimen[pos_idx]
      }
    }
    BART_output <- list(X_selected = X_selected,
                        head_selected = head_selected,
                        dimen_selected = dimen_selected)
    return(BART_output)
  }
}
