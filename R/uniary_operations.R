uniaryOperation <- function(BART_output, sin_cos, apply_pos_opt_on_neg_x) {
  # Uniary operations
  data <- BART_output
  names(data) <- c("X", "head", "dimen")
  Uni_opt <- uniary(BART_output, sin_cos, apply_pos_opt_on_neg_x)
  colnames(Uni_opt$X) <- Uni_opt$head

  # Attach input data
  Uni_opt$X_selected <- BART_output$X_selected
  Uni_opt$head_selected <- BART_output$head_selected
  Uni_opt$dimen_selected <- BART_output$dimen_selected

  return(Uni_opt)
}
