descriptorGenerator <- function(data = NULL, opt = "binary", sin_cos = FALSE, apply_pos_opt_on_neg_x = TRUE, verbose = TRUE) {
  if (opt == "binary") {
    if (verbose) cat("Constructing descriptors using binary operators... \n")
    data <- binary(data, sin_cos)
  } else if (opt == "unary") {
    if (verbose) cat("Constructing descriptors using unary operators... \n")
    data <- unary(data, sin_cos, apply_pos_opt_on_neg_x)
  } else {
    if (verbose) cat("Constructing descriptors using all operators... \n")
    descriptor_unary <- unary(data, sin_cos, apply_pos_opt_on_neg_x)
    descriptor_binary <- binary(data, sin_cos)
    data$X <- cbind(descriptor_unary$X, descriptor_binary$X)
    data$head <- c(descriptor_unary$head, descriptor_binary$head)
    data$unit <- c(descriptor_unary$unit, descriptor_binary$unit)
    # data <- dataprocessing(data)
  }
  return(data)
}
