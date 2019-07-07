#'PFm
#'
#'Creates psychometric function model from psychometric function description
#'@param pf psychometric function description of type PF
#'@param predictor Value of stimulus intensity, that should predict ability to distinguish stimulus
#'@param observation Number of observations over one stimulus intensity
#'@param otherData List of data.frame with other data to be included along the generated data of PFm
#'@param type Type of response encoding PC, or yes/no
#'@param noise Should be noise included in level generation
#'@param ... Parameters of noise generation
#'@return PFm model
#'@export

PFm <- function(pf, predictor, observations, otherData=NULL, type=c("PC", "yes/no"), noise=FALSE, ...){
  if(length(predictor) != length(observations)){stop("Number of predictor and aboservations must be the same.")}
  if(!is.null(otherData) && (is(otherData, "data.frame") || is(otherData, "tibble")) && nrow(otherData) != length(observations)){ stop("Number of rows in other data must be the same as number of predictor.")}
  if(!is(pf, "PF")){stop("Object pf must be a PF type.")}

  #predicting the values
  if(noise){  PC <- noisedPredict(pf, predictor, ...)}
  else{ PC <- predict(pf, predictor)}

  # yes/no translation creating data.frame
  if(type == "yes/no"){ data <- PC2YesNo(predictor, observations, PC); PFmFormula <- formula("c(yes,no)~predictor")}
  else if(type=="PC"){data <- data.frame(predictor=predictor, observations=observations, PC); PFmFormula <- formula("c(PC,observations)~predictor")}
  else{stop("Unknown format of type.")}

  #Adding other data TODP
  if(!is.null(otherData)){ data <- dplyr::bind_cols(data,otherData)}

  model <- pf
  model$data <- data
  model$formula <- PFmFormula
  model$type <- type
  model$noise <- noise
  if(noise){
    model$noiseParams <- list(...)
  }

  class(model) <- c("PFm", class(model))

  model$perf_th              <- perf_threshold(model)
  model$imp_th               <- imp_threshold(model)
  model$d_th                 <- dprime_threshold(model)
  model$iqr                  <- iqr(model)
  model$w                    <- w_range(model)
  model$log_likelihood       <- log_likelihood(model)
  model$log_likelihood_ratio <- log_likRatio(model)
  model$pearson_x            <- pearsonx(model)
  model$mse                  <- mse(model)
  ##TODO value, AIC, BIC, ...

  return(model)
}
