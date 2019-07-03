#'@name polynom
#'@rdname polynom
#'@aliases
#'polynom
#'polynom.cdf
#'polynom.pdf
#'
#'@title Polynom
#'@description
#'Polynomial function is a core type function.
#'\cr
#`a ~  params[1]
#'\cr
#`b ~  params[2]
#'\cr
#'It's CDF formula is: y = (x / a) ^ b
#'\cr
#'It's PDF formula per x is: y = (x / a) ^ (b- 1) * b / a
#'\cr
#'It's PDF formula per a is: y = (x / a)^(b + 1) * b * x
#'\cr
#'It's PDF formula per b is: y = (x / a)^(b) * log(x/a)
#'@param x Vector of x parametres a and b
#'@param params polynom function has two parameters \n a \n b
#'@param b adjusts the curve slope
#'
#'@return Vector of result vaues
NULL

#'@rdname polynom
polynom <- function(x, params) {
  UseMethod("polynom")
}
#'@rdname polynom
polynom.orig <- function(x, params) {
  UseMethod("polynom.orig")
}
#'@rdname polynom
polynom.inverse <- function(x, params) {
  UseMethod("polynom.inverse")
}
#'@rdname polynom
polynom.orig.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Polynomial function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return((x / params[1]) ^ params[2])
}
#'@rdname polynom
polynom.orig.pdf_x <- function(x, params) {
  if(length(params) != 2) { warning("Polynomial function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return((x / params[1]) ^ (params[2]- 1) * params[2] / params[1])
}
#'@rdname polynom
polynom.orig.pdf_p1 <- function(x, params) {
  if(length(params) != 2) { warning("Polynomial function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[2] * (x/params[1]) ^ params[2] / params[1])
}
#'@rdname polynom
polynom.orig.pdf_p2 <- function(x, params) {
  if(length(params) != 2) { warning("Polynomial function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(((x / params[1]) ^ params[2]) * log(x / params[1]))
}
#'@rdname polynom
polynom.inverse_x.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Polynomial function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[1] * (x ^ (1/params[2])))
}
#'@rdname polynom
polynom.inverse_x.pdf_x <- function(x, params) {
  if(length(params) != 2) { warning("Polynomial function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(params[1]/params[2] * x ^ (1/params[2]-1))
}
