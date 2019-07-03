#'@name gumbel_l
#'@rdname gumbel_l
#'@aliases
#'gumbel_l
#'gumbel_l.cdf
#'gumbel_l.pdf
#'
#'@title Gumbel_l
#'@description
#'Left gumbel function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = 1 - exp(- exp(x))
#'\cr
#'It's PDF formula is: y = exp(- exp(x) + x)
#'
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues

#'@rdname gumbel_l
gumbel_l <- function(x) {
  UseMethod("gumbel_l")
}
#'@rdname gumbel_l
gumbel_l.orig <- function(x) {
  UseMethod("gumbel_l.orig")
}
#'@rdname gumbel_l
gumbel_l.inverse <- function(x) {
  UseMethod("gumbel_l.inverse")
}
#'@rdname gumbel_l
gumbel_l.orig.cdf <- function(x) {
  return(1 - exp(- exp(x)))
}
#'@rdname gumbel_l
gumbel_l.orig.pdf <- function(x) {
  return(exp(- exp(x) + x))
}
#'@rdname gumbel_l
gumbel_l.inverse.cdf <- function(x) {
  return(log(-log(1 - x)))
}
#'@rdname gumbel_l
gumbel_l.inverse.pdf <- function(x) {
  return(1 / ((x-1)*log(1-x)))
}
