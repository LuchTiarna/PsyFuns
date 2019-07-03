#'Improvement based Threshold
#'
#'@description
#'Estimates threshold level of given function based on given paramters.
#'\cr
#'Improvement based threshold is a point where subject's performance grows the most.
#'@param pf hit percentage of experiment data
#'@return Level of stimuli with highest derivative

imp_threshold <- function(pf){
  #TODO
  if(pf$core == "linear" || pf$core == "ab" || pf$core == "al"){
    if(pf$sigmoid == "quick" || pf$sigmoid == "exponential"){return(NaN)}

    coreName <- paste(pf$core, "inverse_x.cdf",sep=".")

    corei_x <- function(){eval(body(coreName))}
    formals(corei_x) <- formals(coreName)

    th <- corei_x(0, pf$params)
    return(th)
  }

  core_x <- function(){eval(body(paste(pf$core, "orig.cdf",sep=".")))}
  formals(core_x) <- formals(pf$core)
  core_pdfx <- function(){eval(body(paste(pf$core, "orig.pdf_x",sep=".")))}
  formals(core_pdfx) <- formals(pf$core)
  sigmoid_pdfx <- function(){eval(body(paste(pf$sigmoid, "orig.pdf",sep=".")))}
  formals(sigmoid_pdfx) <- formals(pf$sigmoid)

  fn <- function(x=1){
    return(sigmoid_pdfx(core_x(x, pf$params))*core_pdfx(x, pf$params))
  }

  corei_x <- function(){eval(body(paste(pf$core, "inverse_x.cdf",sep=".")))}
  formals(corei_x) <- formals(pf$core)
  sigmoidi_x <- function(){eval(body(paste(pf$sigmoid, "inverse.cdf",sep=".")))}
  formals(sigmoidi_x) <- formals(pf$sigmoid)

  #Constants are chosen, because at and above given levels improvement threshold does not usualy occur
  min <- corei_x(sigmoidi_x(0.0001), pf$params)
  max <- corei_x(sigmoidi_x(0.9999), pf$params)

  th <- optimize(f=fn, lower=min, upper=max, maximum=TRUE)$par

  return(th)
}
