#'Performance based Threshold
#'
#'@description
#'Estimates threshold level of given function based on given paramters.
#'\cr
#'Performance based threshold is a point where subjects performance is halfway between gamma and 1-lambda.
#'@param pf Psychometric function representation the threshold is to be estimated
#'@return Level of stimuli reaching tha halfway of inner function
perf_threshold <- function(pf){

  coreName <- paste(pf$core, "inverse_x.cdf",sep=".")

  corei_x <- function(){eval(body(coreName))}
  formals(corei_x) <- formals(coreName)

  if(pf$sigmoid == "gauss" || pf$sigmoid == "cauchy" || pf$sigmoid == "logistic" || pf$sigmoid == "htan"){
    th <- corei_x(0, pf$params)
  }else if(pf$sigmoid == "gumbel_r"){
    th <- corei_x(-log(log(2)), pf$params)
  }else if(pf$sigmoid == "gumbel_l"){
    th <- corei_x(log(log(2)), pf$params)
  }else if(pf$sigmoid == "quick"){
    th <- corei_x(1, pf$params)
  }else if(pf$sigmoid == "exponential"){
    th <- corei_x(log(2), pf$params)
  }

  return(th)
}
