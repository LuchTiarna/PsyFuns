#'@name al
#'@rdname al
#'@aliases
#'al
#'al.cdf
#'al.pdf
#'
#'@title Linear adjusted function
#'@description
#'AL function is a core type function.
#'\cr
#`a ~  params[1]
#'\cr
#`b ~  params[2]
#'\cr
#'It's CDF formula is: y = (x - a) * b
#'\cr
#'It's PDF formula is: y per x= b
#'\cr
#'It's PDF formula per a is: y = -b
#'\cr
#'It's PDF formula per b is: y = (x-a)
#'
#'@param x Vector of x parametres a and b
#'@param params Function la has two parameters: \n a - threshold parameter \n b slope parameter
#'
#'@return Vector of result vaues
NULL

#'@rdname al
al <- function(x, params) {
  UseMethod("al")
}
#'@rdname al
al.orig <- function(x, params) {
  UseMethod("al.orig")
}
#'@rdname al
al.inverse <- function(x, params) {
  UseMethod("al.inverse")
}
#'@rdname al
al.orig.cdf <- function(x, params) {
  if(length(params) != 2) { warning("AL function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return((x - params[1]) * params[2])
}
#'@rdname al
al.orig.pdf_x <- function(x, params){ #.d_x
  if(length(params) != 2) { warning("AL function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return (params[2])
}
al.orig.pdf_p1 <- function(x, params){
  if(length(params) != 2) { warning("AL function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return (-params[2])
}
al.orig.pdf_p2 <- function(x, params){
  if(length(params) != 2) { warning("AL function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return (x-params[1])
}
#'@rdname al
al.inverse_x.cdf <- function(x, params) {
  if(length(params) != 2) { warning("AL function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(x/params[2] + params[1])
}
#'@rdname al
al.inverse_x.pdf_x <- function(x, params) { #.d_x
  if(length(params) != 2) { warning("AL function has two parameters: a and b"); return(rep(NaN, length(x))) }
  return(1/params[2])
}
