#'@name htan
#'@rdname htan
#'@aliases
#'htan
#'htan.cdf
#'htan.pdf
#'
#'@title Exponetial function
#'@description
#'htan function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = 0.5 + tanh(x)/2)
#'\cr
#'It's PDF formula is: y = 0.5/(cosh(x)^2)
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues
NULL

#'@rdname htan
htan <- function(x) {
  UseMethod("htan")
}
#'@rdname htan
htan.orig <- function(x) {
  UseMethod("htan.orig")
}
#'@rdname htan
htan.inverse <- function(x) {
  UseMethod("htan.inverse")
}
#'@rdname htan
htan.orig.cdf <- function(x) {
  return(0.5 + tanh(x)/2)
}
#'
#'@rdname htan
#'@export
htan.orig.pdf <- function(x) {
  return(0.5/(cosh(x)^2))
}
#'@rdname htan
#'@export
htan.inverse.cdf <- function(x) {
  return(atanh(2*x - 1))
}
#'@rdname htan
htan.inverse.pdf <- function(x) {
  return(0.5/(x-x^2))
}
