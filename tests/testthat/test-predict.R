context("test-predict")

singleFitData <- read.csv("../Data_testing/singleFitData.csv")


model <- list()
model$sigmoid <- as.character(singleFitData[1,1])
model$core <- as.character(singleFitData[1,2])
model$gamma <- singleFitData$gamma[1]
model$lambda <- singleFitData$lambda[1]
model$params <- c(singleFitData$param1[1],singleFitData$param2[1])
class(model) <- "PF"

test_that(
  "Right results",{
    expect_equal(singleFitData$hitPercentage,predict(model, singleFitData$level))
    #    expect_identical(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=multipleFitData_rounded$id),
    #                     fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by="id"))
    #    expect_identical(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=list("sigmoid","core","level_distr_type")),
    #                     fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=list(multipleFitData_rounded$sigmoid,multipleFitData_rounded$core, multipleFitData_rounded$level_distr_type)))
    #    expect_equal(unname(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by="id")[unique(unlist(split(multipleFitData_rounded$id, f=list(multipleFitData_rounded$sigmoid,multipleFitData_rounded$core, multipleFitData_rounded$level_distr_type), drop=TRUE)))]),
    #                 unname(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=list("sigmoid","core","level_distr_type"))))
  }
)
