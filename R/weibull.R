#'@name weibull
#'@rdname weibull
#'@aliases
#'weibull
#'weibullcdf
#'weibull.pdf
#'
#'@title Weibul
#'@description
#'Weibull function is a core type function.
#'\cr
#`m ~  params[1]
#'\cr
#`s ~  params[2]
#'\cr
#'It's CDF formula is: y = 2*m*s*(log(x) - log(m)) / log(2) + log(log(2))
#'\cr
#'It's PDF formula per x is: y = 2*m*s/log(2)/x
#'\cr
#'It's PDF formula per m is: y = 2*s/log(2)*(log(x)-log(m)+1)
#'\cr
#'It's PDF formula per s is: 2*m*(log(x) - log(m))/log(2)
#'@param x Vector of x parametres m and s
#'@param params  Weibull function has two parameters \n m marks the x coordinate where function reaches the midpoint. \n s adjusts the slope of the fuction
#'
#'@return Vector of result vaues
NULL

#'@rdname weibull
weibull <- function(x, params) {
  UseMethod("weibull")
}
#'@rdname weibull
weibull.orig <- function(x, params) {
  UseMethod("weibull.orig")
}
#'@rdname weibull
weibull.inverse <- function(x, params) {
  UseMethod("weibull.inverse")
}
#'@rdname weibull
weibull.orig.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Weibull function has two parameters: m and s"); return(rep(NaN, length(x))) }
  return(2*params[1]*params[2]*(log(x) - log(params[1])) / log(2) + log(log(2)))
}
#'@rdname weibull
weibull.orig.pdf_x <- function(x, params) {
  if(length(params) != 2) { warning("Weibull function has two parameters: m and s"); return(rep(NaN, length(x))) }
  return(2*params[1]*params[2]/log(2)/x)
}
#'@rdname weibull
weibull.orig.pdf_p1 <- function(x, params) {
  if(length(params) != 2) { warning("Weibull function has two parameters: m and s"); return(rep(NaN, length(x))) }
  return(2*params[2]/log(2)*(log(x)-log(p1)+1))
}
#'@rdname weibull
weibull.orig.pdf_p2 <- function(x, params) {
  if(length(params) != 2) { warning("Weibull function has two parameters: m and s"); return(rep(NaN, length(x))) }
  return(2/log(2)*p1*(log(x)-log(p1)))
}
#'@rdname weibull
weibull.inverse_x.cdf <- function(x, params) {
  if(length(params) != 2) { warning("Weibull function has two parameters: m and s"); return(rep(NaN, length(x))) }
  return(exp(log(2)/(2*params[1]*params[2])*(x - log(log(2))) + log(params[1])))
}
#'@rdname weibull
weibull.inverse_x.pdf <- function(x, params) {
  if(length(params) != 2) { warning("Weibull function has two parameters: m and s"); return(rep(NaN, length(x))) }
  return(exp(log(2)/(2*params[1]*params[2])*(x - log(log(2))) + log(params[1]))*log(2)/(2*params[1]*params[2]))
}
