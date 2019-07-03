#'def
#'
#'Default algorithm for fitting parameters of psychometric function. Only inner parameters are fitted, gamma and lambda are fixed and have to be specified.
#'
#'@param data Specifies the data set on which the function will be fitted.  Data have to be formated in specified way - data.frame/tibble (yes, no, predictor columns).
#'@param sigmoid determines the outer shape of the fuction
#'@param core dermines scalling of predictor
#'@param gamma sets the loves boundary of function
#'@param lambda sets the highes boundary of function
#'@param ... specifies the parametres of optim function
#'
#'@return vector of return values
#'@export
#'
def_fixed_gamma_lambda <- function(data, sigmoid, core, gamma=0.05, lambda=0.05, par=NULL, fn=NULL, gr=NULL, ...,
                                   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
                                   lower = -Inf, upper = Inf,
                                   control = list(), hessian = FALSE){

  if(gamma < 0 || gamma > 1) {stop("Gamma must be in interval [0,1).")}
  if(lambda < 0 || lambda > 1) {stop("Lambda must be in interval [0,1).")}
  if(gamma + lambda > 1) {stop("Summ of gamma and lambda must be in interval lesser than 1.")}

  coref <- get(paste(core, ".PsyFuns:::", ".orig.cdf", sep=""))
  corei_x <- get(paste(core, ".PsyFuns:::", ".inverse_x.cdf", sep=""))
  sigmoidf <- get(paste(sigmoid, ".PsyFuns:::", ".orig.cdf",sep=""))
  sigmoidi <- get(paste(sigmoid, ".PsyFuns:::", ".inverse.cdf",sep=""))

  model <- NULL
  if(is.null(fn)){ default_fn=TRUE
  fn <- fn_def_fixed_gamma_lambda
  }else{default_fn=FALSE} #if fn is not specified the most-likelihood function is used

  if(is.null(par)){
    ##inner parameter adjusting
    primPar <- primalParamsDef(sigmoidi, corei_x, data)
    par=c(primPar)
  } #TODO

  fit <- NULL
  if(default_fn){
    fit <- tryCatch({stats::optim(par=par, fn=fn, gr=gr, gamma, lambda, sigmoidf, sigmoidi, coref, corei_x, data, method=method, lower=lower, upper=upper, control=control)})
  }else{
    fit <- tryCatch({stats::optim(par=par, fn=fn, gr=gr, gamma, lambda, ..., method=method, lower=lower, upper=upper, control=control)})
  }

  if(!is.list(fit)){return(fit)}

  model <- append(fit, list(sigmoid=sigmoid, core=core, gamma=gamma, lambda=lambda, params=c(fit$par)))
  model$par <- NULL
  class(model) <- c("PF",class(model))

  model$startMidpoint <- corei_x(sigmoidi(0.5), primPar)

  return(model)
}

fn_def_fixed_gamma_lambda <- function(params, gamma, lambda, sigmoidf, sigmoidi, coref, corei_x, data){
  y <- PFunction(sigmoidf, coref, data$predictor, gamma, lambda, params)
  #checking if function is increasing
  ymax <- y[base::which.max(data$predictor)]
  ymin <- y[base::which.min(data$predictor)]
  if(ymin > ymax){  return(Inf)  }

  #checking if function is unfolds in range of predictor if halfway is in the range of predictor
  midpoint <-  corei_x(sigmoidi(0.5), params)
  if(midpoint >= max(data$predictor) || midpoint <= min(data$predictor)){return(Inf)}

  if(length(y) != length(data$yes) || length(data$yes) != length(data$no))
  {warning("All vectors must have the same length."); return(NaN)}
  pe <- data$yes*base::log(y)
  pe <- pe + data$no*base::log(1-y)
  pe <- -sum(pe)

  if(is.nan(pe)){return(Inf)}
  return(pe)
}
