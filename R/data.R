#' Single-Atom Catalysis Data
#'
#' @format A list with 4 objects:
#' \describe{
#'   \item{X}{Primary feature matrix: physical properties of transition metals and oxide supports}
#'   \item{y}{Reponse variable: binding energy of metal/oxide pairs}
#'   \item{head}{Column names of X}
#'   \item{unit}{Unit of columns of X}
#' }
"catalysis"

#' iBART Real Data Result
#'
#' @description
#' iBART result in the real data vignette
#'
#' @format A list of iBART outputs
#' \describe{
#'   \item{iBART_model}{A cv.glmnet object storing the iBART selected model}
#'   ...
#' }
"iBART_real_data"

#' iBART Simulation Result
#'
#' @description
#' iBART result in the simulation vignette
#'
#' @format A list of iBART outputs
#' \describe{
#'   \item{iBART_model}{A cv.glmnet object storing the iBART selected model}
#'   ...
#' }
"iBART_sim"
