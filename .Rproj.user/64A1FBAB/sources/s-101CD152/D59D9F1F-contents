#'Primal parameters
#'
#'Default algorithm for adjusting initial parameters of core of psychometric psychometric function.
#'
#'@param sigmoidi inverse of psychometric function sigmoid
#'@param corei_x inverse for psychometric function core
#'@param data data for derermining the outer shape of the fuction
#'

primalParamsDef <- function(sigmoidi, corei_x, data){
  specimen_points <- c(sigmoidi(0.01),sigmoidi(0.5),sigmoidi(0.99))
  matched_points <- c(min(data$predictor),mean(c(min(data$predictor),max(data$predictor))), max(data$predictor))

  fsett <- function(params){
    transduced_points <- corei_x(specimen_points, params)
    point_distance <- (transduced_points - matched_points)^2

    return(log(sum(point_distance)))
  }

  limit <- 10
  params_inner_innit <- optim(par=rep(1,2), fn=fsett)
  midpoint <- corei_x(specimen_points[2], params_inner_innit$par)
  while((midpoint < matched_points[1] || midpoint > matched_points[3]) && limit > 0){
    limit <- limit - 1
    params_inner_innit <- optim(par=params_inner_innit, fn=fsett)
    midpoint <- corei_x(specimen_points[2], params_inner_innit$par)
  }

  return(params_inner_innit$par)


}
