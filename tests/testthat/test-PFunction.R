context("test-PFunction")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

singleFitData <- read.csv("../Data_testing/singleFitData.csv")

testthat::test_that(
  "Basic equality",{
    expect_equal(PsyMetFuns:::PFunction("gumbel_r", "ab", singleFitData$level, singleFitData$gamma[1], singleFitData$lambda[1], singleFitData$param1[1], singleFitData$param2[2]), singleFitData$hitPercentage)
  }
)

testthat::test_that(
  "NaN robustness sigmoid functions",{
    expect_equal(PsyMetFuns:::gumbel_l.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gumbel_l.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gumbel_l.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gumbel_l.inverse.pdf(NaN), NaN)

    expect_equal(PsyMetFuns:::gumbel_r.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gumbel_r.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gumbel_r.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gumbel_r.inverse.pdf(NaN), NaN)

    expect_equal(PsyMetFuns:::cauchy.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::cauchy.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::cauchy.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::cauchy.inverse.pdf(NaN), NaN)

    expect_equal(PsyMetFuns:::gauss.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gauss.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gauss.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::gauss.inverse.pdf(NaN), NaN)

    expect_equal(PsyMetFuns:::exponential.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::exponential.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::exponential.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::exponential.inverse.pdf(NaN), NaN)

    expect_equal(PsyMetFuns:::quick.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::quick.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::quick.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::quick.inverse.pdf(NaN), NaN)

    expect_equal(PsyMetFuns:::htan.orig.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::htan.orig.pdf(NaN), NaN)
    expect_equal(PsyMetFuns:::htan.inverse.cdf(NaN), NaN)
    expect_equal(PsyMetFuns:::htan.inverse.pdf(NaN), NaN)

  }
)

