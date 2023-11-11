#' @export
#' @title A helper function to generate unit for iBART input
#'
#' @param unit A vector of unit of the primary features. For example, unit <- c("cm", "s"). Then the unit of \eqn{x1} is centimeter and the unit of \eqn{x2} is second.
#' @param dimension A vector of dimension of the units. For example, unit <- c("cm", "s") and dimension <- c(2, 1) mean that the unit of \eqn{x1} is square centimeter and the unit of \eqn{x2} is second.
#' @return A list that contains unit and dimension information.

generate_unit <- function(unit, dimension) {
  names(dimension) <- unit
  dimen <- list()
  for (i in 1:length(dimension)) {
    dimen[[i]] = dimension[i]
  }
  return(dimen)
}
