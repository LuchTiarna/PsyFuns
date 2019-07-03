#'@name logy
#'@rdname logy
#'@aliases
#'logy
#'logy.cdf
#'logy.pdf
#'
#'@title Logy
#'@description
#'Log function is a core type function. It is the part of PDF.
#'\cr
#`a ~  params[1]
#'\cr
#`b ~  params[2]
#'\cr
#'It's CDF formula is: y = a * log(x) + b
#'\cr
#'It's PDF formula per x is: y = a / x
#'\cr
#'It's PDF formula per a is: y = log(x)
#'\cr
#'It's PDF formula per b is: y = 1
#'@param x Vector of x parametres a and b
#'@param params logy function has two parameters\n a - increases the gradient of the function \n b - defines the origin point
#'@return Vector of result vaues
#'
#'@rdname logy
NULL

logy <- function(x, params) {
  UseMethod("logy")
}
#'@rdname logy
logy.orig <- function(x, params) {
  UseMethod("logy.orig")
}
#'@rdname logy
logy.inverse <- function(x, params) {
  UseMethod("logy.inverse")
}
#'@rdname logy
logy.orig.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Logaritmic function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[1] * log(x) + params[2])
}
#'@rdname logy
logy.orig.pdf_x <- function(x, params) {
  if(length(params) != 2) { warning("Logaritmic function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[1] / x)
}
#'@rdname logy
logy.orig.pdf_p1 <- function(x, params) {
  if(length(params) != 2) { warning("Logaritmic function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(log(x))
}
#'@rdname logy
logy.orig.pdf_p2 <- function(x, params) {
  if(length(params) != 2) { warning("Logaritmic function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(1)
}
#'@rdname logy
logy.inverse_x.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Logaritmic function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(exp((x-params[2])/params[1]))
}
#'@rdname logy
logy.inverse_x.pdf_x <- function(x, params) {
  if(length(params) != 2) { warning("Logaritmic function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(exp((x-params[2])/params[1])*(x/params[1]))
}
