#' @import bartMachine
BART_iter <- function(data = NULL,
                      BART_var_sel_method = "global_se",
                      num_trees = 20,
                      num_burn_in = 10000,
                      num_iterations_after_burn_in = 5000,
                      num_reps_for_avg = 10,
                      num_permute_samples = 50,
                      standardize = TRUE,
                      train_idx = NULL,
                      seed = NULL,
                      iter = 1) {
  data$X <- as.matrix(data$X) # Make sure X is a matrix, not a vector
  if (!is.null(data$X_selected)) data$X_selected <- as.matrix(data$X_selected)
  # data$iBART_gen_size <- c(data$iBART_gen_size, ncol(data$X))
  dat <- trainingSplit(X = data$X, y = data$y, train_idx = train_idx)

  # bartMachine only takes data.frame
  dat$X_train <- if (standardize) as.data.frame(scale(dat$X_train)) else as.data.frame(dat$X_train)
  bart_machine <- bartMachine(X = dat$X_train,
                              y = dat$y_train,
                              num_trees = num_trees,
                              num_burn_in = num_burn_in,
                              num_iterations_after_burn_in = num_iterations_after_burn_in,
                              run_in_sample = FALSE,
                              serialize = FALSE,
                              seed = if (is.null(seed)) NULL else seed,
                              verbose = FALSE)

  var_sel <- var_selection_by_permute(bart_machine,
                                      num_reps_for_avg = num_reps_for_avg,
                                      num_permute_samples = num_permute_samples,
                                      num_trees_for_permute = 20,
                                      plot = FALSE)

  # Store selected index
  if (BART_var_sel_method == "global_max") {
    pos_idx <- sort(var_sel$important_vars_global_max_col_nums)
  } else if (BART_var_sel_method == "local") {
    pos_idx <- sort(var_sel$important_vars_local_col_nums)
  } else {
    pos_idx <- sort(var_sel$important_vars_global_se_col_nums)
  }

  # length(pos_idx) == 0 means no feature is selected
  if (length(pos_idx) == 0) {
    # Check if BART selected any variable in the 1st iter
    if (iter == 1) {
      stop("iBART didn't select any features in the 1st iteration. Please consider setting hold >= 2.")
    }
    # pos_idx <- NULL
    data$X <- data$X_selected # The running X has to be restored to the last selected pool
    data$head <- data$head_selected
    if (!is.null(data$unit)) data$unit <- data$unit_selected

    data$no_sel_count <- data$no_sel_count + 1
    data$X_selected <- data$head_selected <- data$unit_selected <- NULL
    data$iBART_sel_size <- c(data$iBART_sel_size, NA)
    return(data) # early stop
  }

  # If BART selected some variables...
  # Union new selections with previous selections
  data$X_selected <- cbind(data$X_selected, data$X[, pos_idx])
  data$head_selected <- c(data$head_selected, data$head[pos_idx])
  if (!is.null(data$unit)) data$unit_selected <- c(data$unit_selected, data$unit[pos_idx])

  # Remove duplicated data
  temp <- round(data$X_selected, digits = 6)
  dup_index <- duplicated(temp, MARGIN = 2)
  data$X <- data$X_selected <- as.matrix(data$X_selected[, !dup_index])
  data$head <- data$head_selected <- data$head_selected[!dup_index]
  if (!is.null(data$unit)) data$unit <- data$unit_selected <- data$unit_selected[!dup_index]

  # Attach colnames in case ncol(data$X_selected) == 1
  colnames(data$X_selected) <- colnames(data$X) <- data$head

  data$iBART_sel_size <- c(data$iBART_sel_size, length(pos_idx))
  data$no_sel_count <- 0 # Reset no selection counter
  return(data)
}
