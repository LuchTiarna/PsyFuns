#'MSE psychometric model
#'
#'@description
#'Estimates MSE of given model based on given data.
#'@param pfm Psychometric function model
#'@return MSE measuremet of model

mse <- function(pfm){

  formula <- pfm$formula
  data <- pfm$data
  type <- pfm$type

  data <- data.frame(predictor=data[[formula[[3]]]], PC=data[[formula[[2]][[2]]]],observations=data[[formula[[2]][[3]]]]) # creating a suiting representation of data
  if(tolower(type)=="yes/no"){
    #conversing to PC arrangement
    data <- YesNo2PC(data$predictor, data$no, data$yes)
  }else if (tolower(type) != "pc"){
    warning("Unknown data organization. Use \"hitPercentage\" or \"yes/no\" notation.\n");return(NULL)
  }

  y <- predict(pfm, data$predictor)
  y <- ifelse(y >= 1, 1- .Machine$double.neg.eps, y)
  y <- ifelse(y <= 0, .Machine$double.xmin, y)

  pePred <- (data$PC-y)^2
  pePred <- sum(pePred)

  return(pePred)
}
