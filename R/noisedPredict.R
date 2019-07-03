#'noisedPredict
#'
#'Predicts the values of given model of psychometric function based on level of stimulation it also ads a noise to every simultaion.
#'
#'@param pfm Psychometric function model
#'@param x the vector of level values
#'@return vector of model values
#'@export

noisedPredict <- function(pfm, x, noiseDistr=c("normal", "poisson", "exponential", "logistic", "uniform", "binomial"), ..., symetric=FALSE){

  predicted <- predict(pfm, x)

  if(noiseDistr == "binomial"|| noiseDistr=="binom"){
    if(!is(pfm, "PFm")){stop("For generating binomial noise, number of observations is needed. Therefore object must be of type PFm.")}

    binomialPredict <- function(prob, ObsN){
      vec <- rbinom(ObsN, 1, prob)
      return(mean(vec))
    }
    predicted <- mapply(binomialPredict, predicted, pfm$data[[pfm$formula[[2]][[3]]]])

    return(predicted)
  }

  #computing the noise amplitude
  if(noiseDistr=="normal" || noiseDistr=="norm"){
    noise <- rnorm(length(x),...)
  }else if(noiseDistr=="poisson" || noiseDistr=="pois"){
    noise <- rpois(length(x),...)
  }else if(noiseDistr=="exponential" || noiseDistr=="exp"){
    noise <- rexp(length(x),...)
  }else if(noiseDistr=="logistic" || noiseDistr=="logis"){
  noise <- rlogis(length(x),...)
  }else if(noiseDistr=="uniform" || noiseDistr=="unif"){
    noise <- runif(length(x),...)
  }

  #noise direction
  if(symetric) {
    noiseDir <- rbinom(length(x), 1, 0.5)*2 -1
    noise <- noise * noiseDir
    }

  predicted <- predicted + noise
  predicted <- ifelse(predicted > 1, 1, predicted)
  predicted <- ifelse(predicted < 0, 0, predicted)

  return(predicted)
}
