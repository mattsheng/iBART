abc_var_selection_by_permute <- function(X, y, num_reps_for_avg, num_permute_samples, nrep, Ntrees, burnin, top, keep, parallel = FALSE) {
  ## Set up permute mat
  permute_mat <- matrix(NA, nrow = num_permute_samples, ncol = ncol(X))
  colnames(permute_mat) <- colnames(X)

  ## Get avg true variable proportions
  cat("avg")
  var_true_props_avg  <- abc_get_averaged_true_var_props(X = X, y = y,
                                                         num_reps_for_avg = num_reps_for_avg,
                                                         nrep = nrep,
                                                         Ntrees = Ntrees,
                                                         burnin = burnin,
                                                         top = top,
                                                         keep = keep,
                                                         parallel = parallel)

  # now sort from high to low
  var_true_props_avg <- sort(var_true_props_avg, decreasing = TRUE)

  ## Build null permutation distribution
  cat("null")
  for (b in 1:num_permute_samples){
    permute_mat[b, ] <-
      abc_get_null_permute_var_importances(X = X, y = y,
                                           nrep = nrep,
                                           Ntrees = Ntrees,
                                           burnin = burnin,
                                           top = top,
                                           keep = keep,
                                           parallel = parallel)
  }
  cat("\n")

  # sort permute mat
  permute_mat <- permute_mat[, names(var_true_props_avg)]

  ## use local cutoff
  pointwise_cutoffs <- apply(permute_mat, 2, quantile, probs = 0.95)
  local_pos_idx <- which((var_true_props_avg > pointwise_cutoffs) & (var_true_props_avg > 0))
  local_pos_names <- NA
  if (length(local_pos_idx) > 0) {
    local_pos_names <- names(var_true_props_avg)[local_pos_idx]
  }

  ## use global max cutoff
  max_cut <- quantile(apply(permute_mat, 1 ,max), 0.95)
  max_pos_idx <- which((var_true_props_avg >= max_cut) & (var_true_props_avg > 0))
  max_pos_names <- NA
  if (length(max_pos_idx) > 0) {
    max_pos_names <- names(var_true_props_avg)[max_pos_idx]
  }

  ## use global se cutoff
  perm_se <- apply(permute_mat, 2, sd)
  perm_mean <- apply(permute_mat, 2, mean)
  cover_constant <- abc_bisectK(tol = 0.01, coverage = 0.95, permute_mat = permute_mat,
                                x_left = 1, x_right = 20, countLimit = 100,
                                perm_mean = perm_mean, perm_se = perm_se)
  se_cutoff <- perm_mean + cover_constant * perm_se
  se_pos_idx <- which((var_true_props_avg >= se_cutoff) & (var_true_props_avg > 0))
  se_pos_names <- NA
  if (length(se_pos_idx) > 0) {
    se_pos_names <- names(var_true_props_avg)[se_pos_idx]
  }

  results <- list(local_pos_names = local_pos_names,
                  local_pos_idx = local_pos_idx,
                  max_pos_names = max_pos_names,
                  max_pos_idx = max_pos_idx,
                  se_pos_names = se_pos_names,
                  se_pos_idx = se_pos_idx)
}


abc_get_averaged_true_var_props <- function(X, y, num_reps_for_avg, nrep, Ntrees, burnin, top, keep, parallel = FALSE){
  var_props <- rep(0, ncol(X))
  names(var_props) <- colnames(X)
  seeds <- sample.int(1000, num_reps_for_avg)
  for (i in 1:num_reps_for_avg){
    set.seed(seeds[i])
    invisible(capture.output(
      abc_dup <- ABC_TREE(y, X,
                          nrep = nrep,
                          Ntrees = Ntrees,
                          burnin = burnin,
                          keep = keep,
                          parallel = parallel)))
    var_props <- var_props + process.abc(abc_dup, top = top, method = "top")
    cat(".")
  }
  # average over many runs
  return(var_props / num_reps_for_avg)
}

abc_get_null_permute_var_importances <- function(X, y, nrep, Ntrees, burnin, top, keep, parallel = FALSE){
  # permute the responses to disconnect x and y
  y_permuted <- sample(y, replace = FALSE)

  invisible(capture.output(
    abc_perm <- ABC_TREE(y_permuted, X,
                         nrep = nrep,
                         Ntrees = Ntrees,
                         burnin = burnin,
                         keep = keep,
                         parallel = parallel)))

  # Just return the variable proportions
  var_props <- process.abc(abc_perm, top = top, method = "top")
  cat(".")
  return(var_props)
}

abc_bisectK <- function(tol, coverage, permute_mat, x_left, x_right, countLimit, perm_mean, perm_se){
  count <- 0
  guess <- mean(c(x_left, x_right))
  while ((x_right - x_left) / 2 >= tol & count < countLimit){
    empirical_coverage = mean(sapply(1 : nrow(permute_mat), function(s){all(permute_mat[s,] - perm_mean <= guess * perm_se)}))
    if (empirical_coverage - coverage == 0){
      break
    } else if (empirical_coverage - coverage < 0){
      x_left <- guess
    } else {
      x_right <- guess
    }
    guess <- mean(c(x_left, x_right))
    count <- count + 1
  }
  guess
}
