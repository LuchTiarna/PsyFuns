#'fitPF
#'
#'Provides the frame for psycometrics functions. Combines the sigmoid and core function.
#'@param predictor the vector of predictor values
#'@param observations vector of number of observations per predictor
#'@param PC proportion of hits over all responses
#'@param inverse specifies, whether to compute the inverse function
#'
#'@return dd data.frame containing predictor and number of yes/no respones

PC2YesNo <- function(predictor, observation, PC){

  dd <- data.frame(predictor=predictor, no=observation, yes=PC)

  dd$yes <- dd$yes*dd$no
  dd$no  <- dd$no - dd$yes

  return(dd)
}
