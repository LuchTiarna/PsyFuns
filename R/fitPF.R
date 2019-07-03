#'fitPF
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
fitPF <- function(formula, data, sigmoid, core, ..., split_by=NULL, type="yes/no", algorithm=def){

  dd <- data.frame(predictor=data[[formula[[3]]]], yes=data[[formula[[2]][[2]]]],no=data[[formula[[2]][[3]]]]) # creating a suiting representation of data
  if(tolower(type)=="pc"){
    #conversing to yes/no arrangement
    dd <- PC2YesNo(dd$predictor, dd$no, dd$yes)
  }else if (tolower(type) != "yes/no"){
    warning("Unknown data organization. Use \"PC\" or \"yes/no\" notation.\n");return(NULL)
  }

  if(!all((dd$yes) == round(dd$yes)) || !all(dd$no == round(dd$no))){
    if(tolower(type) == "pc"){warning("The multiplication product of Proportion correct and obsNumbers should be an integer, because Yes and No responses counts should be integers.\n")}
    else{ warning("Yes and No responses counts should be integers.\n")}
  }

  #spliting data according to additional parameters
  if(is.null(split_by)){
    dd <- list(dd)
  }else{
    tryDD <- tryCatch({  split(x=dd, f=split_by, drop=TRUE, lex.order = FALSE) })

    if(!is.list(tryDD) || nrow(tryDD[[1]]) == nrow(dd)){
      error <- tryDD
      tryDD <- tryCatch({
        split_by <- as.list(data[unlist(split_by)])
        split(x=dd, f=split_by, drop = TRUE, lex.order = FALSE)
      })
    }
    if(!is.list(tryDD)){ stop(error, tryDD) }
    dd <- tryDD
  }

  if(is.character(algorithm)){
    algorithmName <- algorithm
    algorithm <- function(){eval(body(algorithmName))}
    formals(algorithm) <- formals(algorithmName)
  }else if(is.function(algorithm)){
    algorithm <- algorithm
  }else{ #TODO
    }

  #fits the data using different fixed parameters
  model <- mapply(FUN=algorithm, dd, SIMPLIFY=FALSE, MoreArgs=list(sigmoid=sigmoid, core=core, ...))

  if(is.null(split_by)){model <- model[[1]]} # when data is not splited returns only PF, not list of PF
  return(model)
}
