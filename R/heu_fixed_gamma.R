#'def
#'
#'Heuristically improved algorithm for fitting parameters of psychometric function. Version of algorithm with fixed gamma parameter. Gamma has to be specifed.
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
heu_fixed_gamma <- function(data, sigmoid, core, gamma=0.05,par=NULL, fn=NULL, gr=NULL, ...,
                            method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
                            lower = -Inf, upper = Inf,
                            control = list(), hessian = FALSE){
  model <- NULL

  if(gamma < 0 || gamma > 1) {stop("Gamma must be in interval [0,1).")}

  coref <- get(paste(core, ".orig.cdf", sep=""))
  corei_x <- get(paste(core, ".inverse_x.cdf", sep=""))
  sigmoidf <- get(paste(sigmoid, ".orig.cdf",sep=""))
  sigmoidi <- get(paste(sigmoid, ".inverse.cdf",sep=""))

  if(is.null(fn)){ default_fn=TRUE
  fn <- fn_def_fixed_gamma
  }else{default_fn=FALSE} #if fn is not specified the most-likelihood function is used

  if(is.null(par)){
    ##inner parameter adjusting
    primPar <- primalParamsDef(sigmoidi, corei_x, data)
    par=c(primPar)
    l_up <- sigmoidi(1-.Machine$double.neg.eps)
    l_low <- sigmoidi(min(0.95, max(data$yes/(data$yes+data$no))))
  } #TODO


  fit <- NULL
  if(default_fn){
    fitUpper <- tryCatch({stats::optim(par=c(l_up,par), fn=fn, gr=gr, gamma, max(data$predictor), min(data$predictor), sigmoidf, sigmoidi, coref, corei_x, data, method=method, lower=lower, upper=upper, control=control)})
    fitLower <-tryCatch({stats::optim(par=c(l_low,par), fn=fn, gr=gr, gamma, max(data$predictor),min(data$predictor), sigmoidf, sigmoidi, coref, corei_x, data, method=method, lower=lower, upper=upper, control=control)})

    #primal guards
    guard_u <- max(corei_x(sigmoidi(0.5), fitUpper$par[-c(1)]),corei_x(sigmoidi(0.5), fitLower$par[-1]))
    guard_l <- min(corei_x(sigmoidi(0.5), fitUpper$par[-c(1)]),corei_x(sigmoidi(0.5), fitLower$par[-1]))

    ## if results of primal fitting are different fitting will continue
    value_old <- max(fitUpper$value, fitLower$value)
    value_new <- min(fitUpper$value, fitLower$value)

    countdown <- 20
    while (value_old > value_new && countdown > 0) {
      countdown <- countdown - 1
      fitUpper_old <- fitUpper
      fitLower_old <- fitLower
      value_old <- value_new
      parUp <- c(l_up,fitLower_old$par[-c(1)])
      parLow <- c(l_low,fitUpper_old$par[-c(1)])
      fitUpper <- tryCatch({stats::optim(par=parUp,  fn=fn, gr=gr, gamma, guard_u, guard_l, sigmoidf, sigmoidi, coref, corei_x, data, method=method, lower=lower, upper=upper, control=control)})
      fitLower <- tryCatch({stats::optim(par=parLow, fn=fn, gr=gr, gamma, guard_u, guard_l, sigmoidf, sigmoidi, coref, corei_x, data, method=method, lower=lower, upper=upper, control=control)})

      guard_u_new <- max(corei_x(sigmoidi(0.5), fitUpper$par[-1]),corei_x(sigmoidi(0.5), fitLower$par[-1]))
      guard_l_new <- min(corei_x(sigmoidi(0.5), fitUpper$par[-1]),corei_x(sigmoidi(0.5), fitLower$par[-1]))

      guard_u <- max(guard_u_new, guard_u)
      guard_l <- max(guard_l_new, guard_l)

      value_new <- min(fitUpper$value, fitLower$value)
    }
    if(fitUpper_old$value < fitLower_old$value){ fit <- fitUpper}
    else{fit <- fitLower }

  }else{
    #TODO
    par=c(sigmoidi(1-la), primPar)
    fit <- tryCatch({stats::optim(par=par, fn=fn, gr=gr, gamma, ..., method=method, lower=lower, upper=upper, control=control)})
  }


  if(!is.list(fit)){return(fit)}

  model <- append(fit, list(sigmoid=sigmoid, core=core, gamma=gamma, lambda=(1-sigmoidf(fit$par[1])), params=c(fit$par[-c(1)])))
  model$par <- NULL
  class(model) <- c("PF",class(model))

  model$startMidpoint <- corei_x(sigmoidi(0.5), primPar)

  return(model)
}

fn_def_fixed_gamma <- function(params, gamma, guard_u, guard_l, sigmoidf, sigmoidi, coref, corei_x, data){
  lambda <- 1 - sigmoidf(params[1])

  if(lambda<=0 || (gamma+lambda >= 1)) {return(Inf)}
  y <- PFunction(sigmoidf, coref, data$predictor, gamma, lambda, params[-c(1)])
  if(any(is.na(y))){return(Inf)}

  #checking if function is increasing
  ymax <- y[base::which.max(data$predictor)]
  ymin <- y[base::which.min(data$predictor)]
  if(ymin > ymax){  return(Inf)  }

  #checking if function is unfolds in range of predictor if halfway is in the range of guards
  midpoint <-  corei_x(sigmoidi(0.5), params[-c(1)])
  if(midpoint < guard_l || midpoint > guard_u){return(Inf)}

  if(length(y) != length(data$yes) || length(data$yes) != length(data$no))
  {warning("All vectors must have the same length."); return(NaN)}
  pe <- data$yes*base::log(y)
  pe <- pe + data$no*base::log(1-y)
  pe <- -sum(pe)

  if(is.nan(pe)){return(Inf)}
  return(pe)
}
