#'@name ab
#'@rdname ab
#'@aliases
#'ab
#'ab.cdf
#'ab.pdf
#'
#'@title ab
#'@description
#'AB function is a core type function.
#'\cr
#`a ~  params[1]
#'\cr
#`b ~  params[2]
#'\cr
#'It's CDF formula is: y = (x - a) / b
#'\cr
#'It's PDF formula per x is: y = 1/b
#'\cr
#'It's PDF formula per a is: y = -1/b
#'\cr
#'It's PDF formula per b is: y = (x - a) / b^2
#'
#'@param x Vector of x parametres a and b
#'@param params Function ab has two parameters: \n a - linear parameter \n b nominater parameter
#'
#'@return Vector of result vaues
NULL

#'@rdname ab
ab <- function(x, params) {
  UseMethod("ab")
}
#'@rdname ab
ab.orig <- function(x, params) {
  UseMethod("ab.orig")
}
#'@rdname ab
ab.inverse <- function(x, params) {
  UseMethod("ab.inverse")
}
#'@rdname ab
ab.orig.cdf <- function(x, params) {
  if(length(params) != 2) { warning("ab function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return((x - params[1]) / params[2])
}
#'@rdname ab
ab.orig.pdf_x <- function(x, params){ #.d_x
  if(length(params) != 2) { warning("ab function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return (1 / params[2])
}
ab.orig.pdf_p1 <- function(x, params){
  if(length(params) != 2) { warning("ab function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return (-1 / params[2])
}
ab.orig.pdf_p2 <- function(x, params){
  if(length(params) != 2) { warning("ab function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return ((x-params[1]) / (params[2])^2)
}
#'@rdname ab
ab.inverse_x.cdf <- function(x, params) {
  if(length(params) != 2) { warning("ab function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[2]*x + params[1])
}
#'@rdname ab
ab.inverse_x.pdf_x <- function(x, params) { #.d_x
  if(length(params) != 2) { warning("ab function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[2])
}
