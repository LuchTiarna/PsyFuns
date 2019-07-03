#'Log-Likelihood of psychometric model
#'@description
#'
#'Estimates likelihood of given model based on given data prediction.
#'\cr
#'
#'@param pfm Psychometric function model
#'@return Likelihood value of model

log_likelihood <- function(pfm){

  my_formula <- pfm$formula
  data <- pfm$data
  type <- pfm$type

  data <- data.frame(predictor=data[[my_formula[[3]]]], yes=data[[my_formula[[2]][[2]]]],no=data[[my_formula[[2]][[3]]]]) # creating a suiting representation of data
  if(tolower(type)=="pc"){
    #conversing to yes/no arrangement
    data <- PC2YesNo(data$predictor, data$no, data$yes)
  }else if (tolower(type) != "yes/no"){
    warning("Unknown data organization. Use \"hitPercentage\" or \"yes/no\" notation.\n");return(NULL)
  }

  y <- predict(pfm, data$predictor)

  pe <- log(choose(round(data$yes + data$no), round(data$yes)))
  pe <- pe + data$yes*base::log(y)
  pe <- pe + data$no*base::log(1-y)
  pe <- sum(pe)

  return(pe)
}
