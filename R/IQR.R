#'Interquartile range
#'
#'@description
#'Estimates interquartile range distance of given function based on given paramters.
#'\cr
#'Interquartile range is a distance between 1/4 and 3/4 'up' of inner psychometric function
#'@param pf Psychometric function representation the IQR is to be estimated
#'@return The distance beween lower and upper quartiles of inner function
iqr <- function(pf){
  sigmoidName <- paste(pf$sigmoid, "inverse.cdf", sep=".")
  sigmoidi <- function(){eval(body(sigmoidName))}
  formals(sigmoidi) <- formals(sigmoidName)

  coreName <- paste(pf$core, "inverse_x.cdf",sep=".")
  corei_x <- function(){eval(body(coreName))}
  formals(corei_x) <- formals(coreName)

  iqr <- corei_x(sigmoidi(c(0.25,0.75)),pf$params)
  iqr <- iqr[2]-iqr[1]

  return(iqr)
}
