context("fixef_other_dists")

test_that('generate other than normal', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 10000
  cov_param <- list(dist_fun = c('rt', 'rchisq'),
                    df = c(4, 3), 
                    var_type = c("single", "single"))
  
  fixef <- data.frame(sim_fixef_single(fixed, fixed_vars, n, cov_param))
  
  expect_gt(e1071::kurtosis(fixef$act), 0)
  expect_gt(e1071::skewness(fixef$gpa), 0)
  expect_gt(e1071::kurtosis(fixef$gpa), 0)
  
})

