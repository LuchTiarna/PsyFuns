#'PearsonX2 deviance of psychometric model
#'
#'@description
#'Estimates Pearsons X^2 deviance of given model based on given data.
#'@param pfm Psychometric function model
#'@return PearsonX measuremet of model

pearsonx <- function(pfm){

  formula <- pfm$formula
  data <- pfm$data
  type <- pfm$type

  data <- data.frame(predictor=data[[formula[[3]]]], PC=data[[formula[[2]][[2]]]],observations=data[[formula[[2]][[3]]]]) # creating a suiting representation of data
  if(tolower(type)=="yes/no"){
    #conversing to PC arrangement
    data <- YesNo2PC(data$predictor, data$yes, data$no)
  }else if (tolower(type) != "pc"){
    warning("Unknown data organization. Use \"hitPercentage\" or \"yes/no\" notation.\n");return(NULL)
  }

  y <- predict(pfm, data$predictor)
  y <- ifelse(y >= 1, 1- .Machine$double.neg.eps, y)
  y <- ifelse(y <= 0, .Machine$double.xmin, y)

  pePred <- (data$PC-y)^2
  pePred <- pePred / (y * (1-y))
  pePred <- pePred * data$observations
  pePred <- sum(pePred)

  return(pePred)
}
