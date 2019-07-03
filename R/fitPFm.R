#'fitPFm
#'
#'Estimates parameters of psychometric function based on given data and settings with given algorithm.
#'@param formula Formula to identify important componets of data. Form is c(yes,no)~predictor for yes/no notation and c(PC,Observations)~predictor for Proportion correct notation.
#'@param sigmoid determines the sigmoid of the fuction
#'@param core dermines the core of the function
#'@param ... specifies the parametres for used fitting algorithm
#'@param split_by specifies, whether and by which criteria should data be divided into subgroups (observers, sessions)
#'@param type specifies type of data formating, proportion correct or yes/no
#'@param algrithm specifies the algorithm to be used for fitting psychometric function parameters.
#'
#'@return vector of return values
#'@export
fitPFm <- function(formula, data, sigmoid, core, ..., split_by=NULL, type="yes/no", algorithm=def){
  pf <- fitPF(formula=formula, data=data, sigmoid=sigmoid, core=core, ..., split_by=split_by, type=type, algorithm=algorithm)

  if(is.null(split_by)){
    pfm <- pfCONVpfm(pf, data, formula, type)
  }else{
    tryDD <- tryCatch({  split(x=data, f=split_by, drop=TRUE, lex.order = FALSE) })
    if(!is.list(tryDD) || nrow(tryDD[[1]]) == nrow(data)){
      error <- tryDD
      tryDD <- tryCatch({
        split_by <- as.list(data[unlist(split_by)])
        split(x=data, f=split_by, drop = TRUE, lex.order = FALSE)
      })
    }
    if(!is.list(tryDD)){ stop(error, tryDD) }
    data <- tryDD
    pfm <- mapply(FUN=pfCONVpfm, pf, data, SIMPLIFY=FALSE, MoreArgs = list(formula=formula, type=type))
  }
  return(pfm)
}

pfCONVpfm <- function(pf, data, formula, type){
  pfm <- pf
  pfm$data <- data
  pfm$formula <- formula
  pfm$type <- type
  pfm$perf_th              <- perf_threshold(pfm)
  pfm$imp_th               <- imp_threshold(pfm)
  pfm$d_th                 <- dprime_threshold(pfm)
  pfm$iqr                  <- iqr(pfm)
  pfm$w                    <- w_range(pfm)
  pfm$log_likelihood       <- log_likelihood(pfm)
  pfm$log_likelihood_ratio <- log_likRatio(pfm)
  pfm$pearson_x            <- pearsonx(pfm)
  pfm$mse                  <- mse(pfm)
  class(pfm) <- c("PFm",class(pfm))
  return(pfm)
}
