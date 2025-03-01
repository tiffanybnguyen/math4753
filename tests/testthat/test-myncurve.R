test_that("Mean is correctly assigned", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
})

test_that("Standard deviation is correctly assigned", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$sigma, 5)
})

test_that("Probability is calculated correctly for a known case", {
  result <- myncurve(mu = 0, sigma = 1, a = 1.96)
  expect_equal(result$probability, round(pnorm(1.96, mean=0, sd=1), 4))
})
