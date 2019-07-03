#'@name linear
#'@rdname linear
#'@aliases
#'linear
#'linear.cdf
#'linear.pdf
#'
#'@title Linear
#'@description
#'Linear function is a core type function.
#'\cr
#`a ~  params[1]
#'\cr
#`b ~  params[2]
#'\cr
#'It's CDF formula is: y = a * x + b
#'\cr
#'It's PDF formula per x is: y = a
#'\cr
#'It's PDF formula per a is: y = x
#'\cr
#'It's PDF formula per b is: y = 1
#'
#'@param x Vector of x parametres a and b
#'@param params Linear function has to paramters: \n a - relative paramter \n b - absolute paramter
#'
#'@return Vector of result vaues
NULL

#'@rdname linear
linear <- function(x, params) {
  UseMethod("linear")
}
#'@rdname linear
linear.orig <- function(x, params) {
  UseMethod("linear.orig")
}
#'@rdname linear
linear.inverse <- function(x, params) {
  UseMethod("linear.inverse")
}
#'@rdname linear
linear.orig.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Linear function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[1] * x + params[2])
}
#'@rdname linear
linear.orig.pdf_x <- function(x, params) {
  if(length(params) != 2) { warning("Linear function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[1])
}
#'@rdname linear
linear.orig.pdf_p1 <- function(x, params) {
  if(length(params) != 2) { warning("Linear function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(x)
}
#'@rdname linear
linear.orig.pdf_p2 <- function(x, params) {
  if(length(params) != 2) { warning("Linear function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(1)
}
#'@rdname linear
linear.inverse_x.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Linear function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return((x - params[2])/params[1])
}
#'@rdname linear
linear.inverse_x.pdf_x <- function(x, params) {#.d_x
  if(length(params) != 2) { warning("Linear function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(1/params[1])
}
