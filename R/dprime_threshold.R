#'D-prime based Threshold
#'
#'@description
#'Estimates threshold level of given function based on given paramters.
#'\cr
#'Improvement based threshold is a point where subject's performance grows the most.
#'@param pf Psychometric function model
#'@param reference Stimulus level used as base for measuring d-prime distance
#'@param d D-prime distance between two stimuli
#'@return Level of stimuli that is in d distance from point of reference

dprime_threshold <- function(pf, reference=0, d=1, transoform=NULL){
  FA <- predict(pf, reference)
  zFA <- qnorm(FA)
  H <- pnorm(zFA + d)

  if(!is.null(transoform)){

  }

  th <- PsyFuns:::PFunction(pf$sigmoid, pf$core, H, pf$gamma, pf$lambda, pf$params, type = "cdf", inverse = TRUE)

  return(th)
}
