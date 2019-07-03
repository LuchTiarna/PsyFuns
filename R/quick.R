#'@name quick
#'@rdname quick
#'@aliases
#'quick
#'quick.cdf
#'quick.pdf
#'
#'@title Quick function
#'@description
#'Quick function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = 1 - 2^(-x))
#'\cr
#'It's PDF formula is: y = log(2)*2^(-x))
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues
NULL

#'@rdname quick
quick <- function(x) {
  UseMethod("quick")
}
#'@rdname quick
quick.orig <- function(x) {
  UseMethod("quick.orig")
}
#'@rdname quick
quick.inverse <- function(x) {
  UseMethod("quick.inverse")
}
#'@rdname quick
quick.orig.cdf <- function(x) {
  return(1 - 2^(-x))
}
#'
#'@rdname quick
#'@export
quick.orig.pdf <- function(x) {
  return(2^(-x) * log(2))
}
#'@rdname quick
#'@export
quick.inverse.cdf <- function(x) {
  return(-log2(1-x))
}
#'@rdname quick
quick.inverse.pdf <- function(x) {
  return(1 / (1-x) / log(2))
}
