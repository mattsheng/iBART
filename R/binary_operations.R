binaryOperation <- function(BART_output, sin_cos) {
  # Binary operations
  data <- BART_output
  names(data) <- c("X", "head", "dimen")
  Bi_opt <- binary(BART_output, sin_cos)
  colnames(Bi_opt$X) <- Bi_opt$head

  # Attach input data
  Bi_opt$X_selected <- BART_output$X_selected
  Bi_opt$head_selected <- BART_output$head_selected
  Bi_opt$dimen_selected <- BART_output$dimen_selected

  return(Bi_opt)
}
