descriptorGenerator <- function(data = NULL, opt = "binary", sin_cos = FALSE, apply_pos_opt_on_neg_x = TRUE, verbose = TRUE) {
  if (opt == "binary") {
    if (verbose) cat("Constructing descriptors using binary operators... \n")
    data <- binary(data, sin_cos)
  } else if (opt == "unary") {
    if (verbose) cat("Constructing descriptors using unary operators... \n")
    data <- unary(data, sin_cos, apply_pos_opt_on_neg_x)
  } else {
    if (verbose) cat("Constructing descriptors using all operators... \n")
    data_unary <- unary(data, sin_cos, apply_pos_opt_on_neg_x)
    data_binary <- binary(data, sin_cos)
    data$X <- cbind(data_unary$X, data_binary$X)
    data$name <- c(data_unary$name, data_binary$name)
    data$unit <- cbind(data_unary$unit, data_binary$unit)
    data <- dataprocessing(data)
  }
  return(data)
}
