test_that("blbglm() finds logstic regression model using blb with option for parallelization", {
  suppressWarnings(fit<-blbglm_par(am ~ hp, data= mtcars, m=3, B=100))
  expect_s3_class(fit, "blbglm")
  co <- coef(fit)
  expect_equal(length(co), 2)
})
