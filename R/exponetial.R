#'@name exponetial
#'@rdname exponetial
#'@aliases
#'exponetial
#'exponetial.cdf
#'exponetial.pdf
#'
#'@title Exponetial function
#'@description
#'Exponential function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = 1 - exp(-x))
#'\cr
#'It's PDF formula is: y = exp(-x))

#'@param x Vector of x parametres
#'@return Vector of result vaues

#'@rdname exponential
exponential <- function(x) {
  UseMethod("exponential")
}
#'@rdname exponential
exponential.orig <- function(x) {
  UseMethod("exponential.orig")
}
#'@rdname exponential
exponential.inverse <- function(x) {
  UseMethod("exponential.inverse")
}
#'@rdname exponential
exponential.orig.cdf <- function(x) {
  return(1 - exp(-x))
}
#'
#'@rdname exponential
exponential.orig.pdf <- function(x) {
  return(exp(-x))
}
#'@rdname exponential
exponential.inverse.cdf <- function(x) {
  return(- log(1-x))
}
#'@rdname exponential
exponential.inverse.pdf <- function(x) {
  return(1 / (1-x))
}
