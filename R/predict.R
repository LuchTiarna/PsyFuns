#'predict
#'
#'Predicts the values of given model of psychometric function based on level of stimulation.
#'@param object PF, or PFm to predict results with
#'@param x the vector of predictor (stimulus level) values
#'@return vector of model values
#'@export
predict.PF <- function(object, x=c(), ...){
            if(!is.numeric(x)){ warning("x must be numeric"); return(NULL) }

            result <- PsyFuns:::PFunction(
                                object$sigmoid,
                                object$core,
                                x,
                                object$gamma,
                                object$lambda,
                                object$params
            )
            return(result)
}




