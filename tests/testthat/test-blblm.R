test_that("blblm() finds linear regression model using blb with option for parallelization", {
  fit<-blblm_par(mpg ~ wt * hp, data= mtcars, m=3, B=100)
  expect_s3_class(fit, "blblm")
  co <- coef(fit)
  expect_equal(length(co), 4)
})
