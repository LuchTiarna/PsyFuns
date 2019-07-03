#'@name gumbel_r
#'@rdname gumbel_r
#'
#'@title Gumbel_r
#'@description
#'Right gumbel function is a sigmoid type function.
#'\cr
#'It's CDF formula is: exp(- exp(-x))
#'\cr
#'It's PDF formula is: y = exp(- exp(-x) - x)
#'
#'@param x Vector of x parametres
#'@return Vector of result vaues

#'@rdname gumbel_r
gumbel_r <- function(x) {
  UseMethod("gumbel_r")
}
#'@rdname gumbel_r
gumbel_r.orig <- function(x) {
  UseMethod("gumbel_r.orig")
}
#'@rdname gumbel_r
gumbel_r.inverse <- function(x) {
  UseMethod("gumbel_r.inverse")
}
#'@rdname gumbel_r
gumbel_r.orig.cdf <- function(x) {
  return(exp(- exp(-x)))
}
#'@rdname gumbel_r
gumbel_r.orig.pdf <- function(x) {
  return(exp(- exp(-x) - x))
}
#'@rdname gumbel_r
gumbel_r.inverse.cdf <- function(x) {
  return(-log(-log(x)))
}
#'@rdname gumbel_r
gumbel_r.inverse.pdf <- function(x) {
  return(-1/(x*log(x)))
}
