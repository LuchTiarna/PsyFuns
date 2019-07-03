#'Log-Likelihood deviance of psychometric model
#'
#'@description
#'Estimates likelihood ratio/deviance of given model and ideal model based on given data.
#'@param pfm Psychometric function model
#'@return Value of likelihood ratio

log_likRatio <- function(pfm){

  formula <- pfm$formula
  data <- pfm$data
  type <- pfm$type

  data <- data.frame(predictor=data[[formula[[3]]]], yes=data[[formula[[2]][[2]]]],no=data[[formula[[2]][[3]]]]) # creating a suiting representation of data
  if(tolower(type)=="pc"){
    #conversing to yes/no arrangement
    data <- PC2YesNo(data$predictor, data$no, data$yes)
  }else if (tolower(type) != "yes/no"){
    warning("Unknown data organization. Use \"hitPercentage\" or \"yes/no\" notation.\n");return(NULL)
  }

  y <- predict(pfm, data$predictor)
  pePred <- data$yes*base::log(y)
  pePred <- pePred + data$no*base::log(1-y)
  pePred <- sum(pePred)


  datR <- data$yes / (data$yes + data$no)
  datR <- ifelse(datR >= 1, 1- .Machine$double.neg.eps, datR)
  datR <- ifelse(datR <= 0, .Machine$double.xmin, datR)

  peMax <- data$yes*base::log(datR)
  peMax <- peMax + data$no*base::log(1-datR)
  peMax <- sum(peMax)


  return(2 * (peMax - pePred))
}
