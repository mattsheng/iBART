unaryOperation <- function(BART_output, sin_cos, apply_pos_opt_on_neg_x) {
  # Unary operations
  Uni_opt <- unary(BART_output, sin_cos, apply_pos_opt_on_neg_x)
  colnames(Uni_opt$X) <- Uni_opt$head

  # Attach input data
  Uni_opt$X_selected <- BART_output$X_selected
  Uni_opt$head_selected <- BART_output$head_selected
  Uni_opt$dimen_selected <- BART_output$dimen_selected

  return(Uni_opt)
}
