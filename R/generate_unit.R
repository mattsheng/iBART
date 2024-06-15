#' @export
#' @title A helper function to generate unit for iBART input
#'
#' @param X The design matrix X containing all the primary features
#' @param unit A vector of unit of the primary features. For example, unit <- c("cm", "s"). Then the unit of \eqn{x1} is centimeter and the unit of \eqn{x2} is second.
#' @param dimension A vector of dimension of the units. For example, unit <- c("cm", "s") and dimension <- c(2, 1) mean that the unit of \eqn{x1} is square centimeter and the unit of \eqn{x2} is second.
#' @return A matrix whose rows are units and columns are covariates.

# generate_dimension <- function(unit, dimension) {
#   names(dimension) <- unit
#   dimen <- list()
#   for (i in 1:length(dimension)) {
#     dimen[[i]] = dimension[i]
#   }
#   return(dimen)
# }


generate_unit <- function(X, unit, dimension) {
  ### Generate Dimension Matrix ###
  p.var <- ncol(X)
  p.dimen <- length(unique(unit))

  dimen <- matrix(0, nrow = p.dimen, ncol = p.var)
  rownames(dimen) <- unique(unit)
  colnames(dimen) <- colnames(X)

  for (i in 1:p.var) {
    dimen[unit[i], i] <- dimension[i]
  }
  return(dimen)
}
