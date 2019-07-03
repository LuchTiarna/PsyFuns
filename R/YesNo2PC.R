#'YesNo2PC
#'
#'Converts data of psychometric function in yes/no format to PC format.
#'@param predictor the vector of predictor values
#'@param yes number yes answers per predictor value
#'@param no number no answers per predictor value
#'
#'@return dd data.frame containing predictor and number of yes/no respones

YesNo2PC <- function(predictor, yes, no){

  dd <- data.frame(predictor=predictor, observations=no, PC=yes)

  dd$observations <- dd$observations + dd$PC
  dd$PC <- dd$PC/dd$observations


  return(dd)
}
