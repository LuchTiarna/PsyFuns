#'deoptim
#'
#'Algorithm using deoptim method to increase precision of fitting psychometric function parameters . Version of algorithm with fixed gamma parameter. Gamma has to be specifed.
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
deoptim_fixed_gamma <- function(data, sigmoid, core, gamma=0.05,par=NULL, fn=NULL, gr=NULL, ...,
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

    par_u <- fitUpper$par
    par_l <- fitLower$par

    lower <- ifelse(par_u > par_l, par_l, par_u)
    upper <- ifelse(par_u > par_l, par_u, par_l)

    ## if results of primal fitting are different fitting will continue

    fit <- DEoptim::DEoptim(fn=fn, lower=lower, upper=upper,
                            control=DEoptim::DEoptim.control(itermax = 300, trace = FALSE),
                            gamma, guard_u, guard_l, sigmoidf, sigmoidi, coref, corei_x, data)

  }else{
    #TODO
    par=c(sigmoidi(1-.Machine$double.eps), primPar)
    fit <- tryCatch({stats::optim(par=par, fn=fn, gr=gr, gamma, ..., method=method, lower=lower, upper=upper, control=control)})
  }


  if(!is.list(fit)){return(fit)}

  model <- append(fit, list(sigmoid=sigmoid, core=core, gamma=gamma, lambda=(1-sigmoidf(fit$optim$bestmem[c(1)])), params=c(fit$optim$bestmem[-c(1)])))
  model$par <- NULL
  class(model) <- c("PF",class(model))

  model$startMidpoint <- corei_x(sigmoidi(0.5), primPar)

  return(model)
}
