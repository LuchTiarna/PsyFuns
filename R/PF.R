#'PFunction
#'
#'Provides the frame for psycometrics functions. Combines the sigmoid and core function.
#'@param sigmoid determines the sigmoid of the fuction
#'@param core dermines the core of the function
#'@param gama sets the loves boundary of function
#'@param lambda sets the highes boundary of function
#'@param params specifies inner parametres of core function
#'@return vector of return values
#'@export

PF <- function(sigmoid, core, gamma, lambda, params){
  model <- list()

  if(is.character(sigmoid)){model$sigmoid <- sigmoid}
  else{stop("Sigmoid must be a character.")}
  if(is.character(core)){model$core <- core}
  else{stop("Sigmoid must be a character.")}

  if(is.numeric(gamma) && gamma < 1 && gamma >= 0){model$gamma <- gamma}
  else{stop("Gamma must be numeric and in interval [0,1).")}
  if(is.numeric(lambda) && lambda < 1 && lambda >= 0){model$lambda <- lambda}
  else{stop("Lambda must be numeric and in interval [0,1).")}

  if(gamma + lambda >= 1) stop("Summ of gamma and lambda must not be bigger, than 1.")

  if(is.numeric(params)){model$params <- params}
  else{stop("Parametrs of function must be numeric.")}

  class(model) <- c("PF",class(model))



  return(model)
}
