#'def
#'
#'Default algorithm for fitting parameters of psychometric function. Version of algorithm with fixed gamma parameter. Gamma has to be specifed.
#'
#'@param data Specifies the data set on which the function will be fitted.  Data have to be formated in specified way - data.frame/tibble (yes, no, predictor columns).
#'@param gamma sets the loves boundary of function
#'@param sigmoid determines the outer shape of the fuction
#'@param core dermines scalling of predictor
#'@param ... specifies the parametres of optim function
#'
#'@return vector of return values
#'@export
#'
def <- function(data, sigmoid, core, par=NULL, fn=NULL, gr=NULL, ...,
                method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
                lower = -Inf, upper = Inf,
                control = list(), hessian = FALSE){
  model <- NULL

  coref <- get(paste(core, ".PsyFuns:::", ".orig.cdf", sep=""))
  corei_x <- get(paste(core, ".PsyFuns:::", ".inverse_x.cdf", sep=""))
  sigmoidf <- get(paste(sigmoid, ".PsyFuns:::", ".orig.cdf",sep=""))
  sigmoidi <- get(paste(sigmoid, ".PsyFuns:::", ".inverse.cdf",sep=""))

  if(is.null(fn)){ default_fn=TRUE
  fn <- fn_def
  }else{default_fn=FALSE} #if fn is not specified the most-likelihood function is used
  if(is.null(par)){

    ##gamma, lambda adjusting
      ga <- min(data$yes / (data$yes + data$no))
      la <- max(data$yes / (data$yes + data$no))

      if(ga <= 0){ ga <- .Machine$double.xmin}
      if(ga > 0.89){ ga <- 0.89}
      if(la < 0.90){ la <- 0.90}
      if(la >= 1){ la <- 1 - .Machine$double.neg.eps}

      ##inner parameter adjusting
      primPar <- primalParamsDef(sigmoidi, corei_x, data)

    par=c(sigmoidi(ga),sigmoidi(la), primPar)
  } #TODO

  fit <- NULL
  if(default_fn){
    fit <- tryCatch({stats::optim(par=par, fn=fn, gr=gr, sigmoidf, sigmoidi, coref, corei_x, data, method=method, lower=lower, upper=upper, control=control)})
  }else{
    fit <- tryCatch({stats::optim(par=par, fn=fn, gr=gr, ..., method=method, lower=lower, upper=upper, control=control)})
  }
  if(!is.list(fit)){return(fit)}

  model <- append(fit, list(sigmoid=sigmoid, core=core, gamma=sigmoidf(fit$par[1]), lambda=(1-sigmoidf(fit$par[2])), params=c(fit$par[-c(1,2)])))
  model$par <- NULL
  class(model) <- c("PF",class(model))

  model$startMidpoint <- corei_x(sigmoidi(0.5), primPar)

  return(model)
}


fn_def <-  function(params, sigmoidf, sigmoidi, coref, corei_x, data){
  gamma <- sigmoidf(params[1])
  lambda <- 1 - sigmoidf(params[2])

  if(gamma<0 || lambda<0 || lambda > 0.11 || gamma > 0.90 || (gamma+lambda > 1)) {return(Inf)}
  y <- PFunction(sigmoidf, coref, data$predictor, gamma, lambda, params[-c(1,2)])
  #checking if function is increasing
  ymax <- y[base::which.max(data$predictor)]
  ymin <- y[base::which.min(data$predictor)]
  if(ymin > ymax){  return(Inf)  }

  #checking if function is unfolds in range of predictor if halfway is in the range of predictor
  midpoint <-  corei_x(sigmoidi(0.5), params[-c(1,2)])
  if(midpoint >= max(data$predictor) || midpoint <= min(data$predictor)){return(Inf)}

  if(length(y) != length(data$yes) || length(data$yes) != length(data$no))
  {warning("All vectors must have the same length."); return(NaN)}
  pe <- data$yes*base::log(y)
  pe <- pe + data$no*base::log(1-y)
  pe <- -sum(pe)

  if(is.nan(pe)){return(Inf)}
  return(pe)
}
