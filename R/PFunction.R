#'PFunction
#'
#'Function used to utilize all versions of psychometric function . Combines the sigmoid and core function.
#'@param sigmoid determines the sigmoid of the fuction
#'@param core dermines the core of the function
#'@param x the vector of level values
#'@param gama sets the loves boundary of function
#'@param lambda sets the highes boundary of function
#'@param type specifies, whether function is CDF of PDF type
#'@param inverse specifies, whether to compute inversion of specified function
#'@param ... specifies the parametres of core function
#'@return vector of return values

PFunction <- function(sigmoid, core, x, gamma, lambda, ... , type="cdf", inverse=FALSE, corePdf=NULL){
  gamma <-  as.double(gamma)
  lambda <- as.double(lambda)
  params <- c(...)


  if(gamma < 0) {warning("Gamma must be a least 0."); return(rep(NaN, length(x)))}
  if(lambda < 0) {warning("Lambda must be a least 0."); return(rep(NaN, length(x)))}
  if((gamma + lambda) > 1) {warning("Summ of gamma and lambda must be lesser than 1."); return(rep(NaN, length(x)))}
  type <- tolower(type)
  if(inverse && type !="cdf"){stop("Inverse is avaible only for cdf type function.")}

  if(is.function(sigmoid) && is.function(core)){
    sigmoidf <- sigmoid
    coref <- core
    if(tolower(substr(type,1,3))=="pdf" && !is.function(corePdf)){
      stop("The link for core pdf function is missing.")
      }
  }
  else if(is.character(sigmoid) && is.character(core)){
    sigmoidName <- sigmoid
    coreName <- core

    if(!inverse){
      sigmoidName <- paste(sigmoidName, "orig",sep=".")
      coreName <- paste(coreName, "orig",sep=".")
    }else{
      sigmoidName <- paste(sigmoidName, "inverse",sep=".")
      coreName <- paste(coreName, "inverse_x",sep=".")
    }

    if(substr(type,1,3)=="cdf"){
      coref <- get(paste(coreName , type, sep="."))
      sigmoidf <- get(paste(sigmoidName, type, sep="."))

    }else if(substr(type,1,3)=="pdf"){
      coref <- get(paste(coreName, "cdf", sep="."))
      corePdf <- get(paste(coreName, type, sep="."))
      sigmoidf <- get(paste(sigmoidName, "cdf", sep="."))
      sigmoidPdf <- get(paste(sigmoidName, "pdf", sep="."))
    }

  }else{#!ADD ERROR!#
  }

  if(substr(type,1,3)=="cdf"){
    if(!inverse){
      y <- gamma + (1 - gamma - lambda) * sigmoidf(coref(x, params))
    }else{
      x <- ifelse(x < gamma || x > 1 - lambda, NaN, x)
      y <- coref(sigmoidf((x-gamma)/(1-lambda-gamma)), params)
    }
  }else if(substr(type,1,3)=="pdf"){
    if(substr(type,5,5)=="g"){
      y <- 1 - sigmoidf(coref(x, params))
    }else if(substr(type,5,5)=="l"){
      y  <-  - sigmoidf(coref(x, params))
    }else{
      y <- (1 - gamma - lambda) * sigmoidPdf(coref(x, params))*corePdf(x, params)
    }
  }else{
    warning("invalid function type, must be either cdf or pdf");return(rep(NaN, length(x)))
  }
  return(y)
}
