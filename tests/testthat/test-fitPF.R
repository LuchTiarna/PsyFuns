context("test-fitPF")

#singleFitData <- read.csv("../Data_testing/singleFitData.csv")
singleFitData_rounded <- read.csv("../Data_testing/singleFitData_rounded.csv")
multipleFitData_rounded <- read.csv("../Data_testing/multipleFitData_rounded.csv")


#testthat::test_that(
#  "Warning if unrounded data",{
#    expect_identical(fitPF(level~c(hitPercentage,obsNumber),singleFitData, "gumbel_l", "ab"))
#    expect_identical(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=multipleFitData_rounded$id),
#                     fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by="id"))
#    expect_identical(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=list("sigmoid","core","level_distr_type")),
#                     fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=list(multipleFitData_rounded$sigmoid,multipleFitData_rounded$core, multipleFitData_rounded$level_distr_type)))
#    expect_equal(unname(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by="id")[unique(unlist(split(multipleFitData_rounded$id, f=list(multipleFitData_rounded$sigmoid,multipleFitData_rounded$core, multipleFitData_rounded$level_distr_type), drop=TRUE)))]),
#                 unname(fitPF(level~c(hitPercentage,obsNumber), multipleFitData_rounded, "gumbel_l", "ab", split_by=list("sigmoid","core","level_distr_type"))))
#
#  }
#)

#library(dplyr)
#piped <- singleFitData_rounded %>% fitPF(level~c(hitPercentage,obsNumber),"gumbel_l", "ab")
#testthat::test_that(
#  "pipe",{
#    expect_equal(fitPF(level~c(hitPercentage,obsNumber),singleFitData_rounded, "gumbel_l", "ab"),
#                 piped)
#  }
#)

test_that(
  "Checking data spliting",{
    expect_equal(is(fitPF(c(hitPercentage,obsNumber)~level, singleFitData_rounded, "gumbel_l", "ab", type="PC"), "PF"), TRUE)
    expect_identical(fitPF(c(hitPercentage,obsNumber)~level, multipleFitData_rounded, "gumbel_l", "ab", type="PC", split_by=multipleFitData_rounded$id),
                     fitPF(c(hitPercentage,obsNumber)~level, multipleFitData_rounded, "gumbel_l", "ab", type="PC",split_by="id"))
    expect_identical(fitPF(c(hitPercentage,obsNumber)~level, multipleFitData_rounded, "gumbel_l", "ab", type="PC", split_by=list("sigmoid","core","level_distr_type")),
                     fitPF(c(hitPercentage,obsNumber)~level, multipleFitData_rounded, "gumbel_l", "ab", type="PC", split_by=list(multipleFitData_rounded$sigmoid,multipleFitData_rounded$core, multipleFitData_rounded$level_distr_type)))
    expect_equal(unname(fitPF(c(hitPercentage,obsNumber)~level, multipleFitData_rounded, "gumbel_l", "ab", type="PC", split_by="id")[unique(unlist(split(multipleFitData_rounded$id, f=list(multipleFitData_rounded$sigmoid,multipleFitData_rounded$core, multipleFitData_rounded$level_distr_type), drop=TRUE)))]),
                 unname(fitPF(c(hitPercentage,obsNumber)~level, multipleFitData_rounded, "gumbel_l", "ab", type="PC", split_by=list("sigmoid","core","level_distr_type"))))
  }
)

test_that(
  "Checking fixing parameters",{
    expect_error(fitPF(c(hitPercentage,obsNumber)~level,data=singleFitData_rounded, sigmoid="gumbel_l", core="ab", gamma=-0.5, type="PC", algorithm = def_fixed_gamma),
                 "Gamma must be in interval [0,1).", fixed=TRUE)
    expect_equal(0.5,
                 fitPF(c(hitPercentage,obsNumber)~level,data=singleFitData_rounded, sigmoid="gumbel_l", core="ab", gamma=0.5, type="PC", algorithm = "def_fixed_gamma")$gamma );
    expect_equal(c(0.5,0.05),
                 {res <- fitPF(c(hitPercentage,obsNumber)~level,data=singleFitData_rounded, sigmoid="gumbel_l", core="ab", gamma=0.5, lambda=0.05, type="PC", algorithm = def_fixed_gamma_lambda); c(res$gamma,res$lambda)});
    #    expect_equal(length(fitPF(level~c(hitPercentage,obsNumber),data=singleFitData_rounded, sigmoid="gumbel_l", core="ab")), 0.5);
  }
)


#library(psyphy)
#test_that(
#  "Fitting against psyphy",{
#    #expect_equal(1, )
#  }
#)
