context("fixef_other_dists")

test_that('generate other than normal', {
  set.seed(2)
  fixed <- ~ 1 + act + gpa + sat
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 10000
  cov_param <- list(dist_fun = c('rt', 'rchisq', 'rnorm'),
                    var_type = c("single", "single", 'single'),
                    opts = list(list(df = 5), list(df = 2), 
                                list(mean = 10, sd = 2)))
  
  fixef <- data.frame(sim_fixef_single(fixed, fixed_vars, n, cov_param))
  
  expect_gt(e1071::kurtosis(fixef$act), 0)
  expect_gt(e1071::skewness(fixef$gpa), 0)
  expect_gt(e1071::kurtosis(fixef$gpa), 0)
  expect_equal(mean(fixef$sat), 10, tolerance = .02)
  expect_equal(sd(fixef$sat), 2, tolerance = .02)
  
})

test_that('generate correlated data different distributions', {
  set.seed(2)
  fixed <- ~ 1 + act + gpa + sat
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 100000
  cov_param <- list(dist_fun = c('rt', 'rchisq', 'rnorm'),
                    var_type = c("single", "single", 'single'),
                    opts = list(list(df = 5), list(df = 2), 
                                list(mean = 10, sd = 2)))
  cor_vars <- c(.6, .4, .1)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- data.frame(sim_fixef_single(fixed, fixed_vars, n, cov_param, 
                                       cor_vars))
  
  expect_equal(cor(fixef[, 2:4]), c_mat, 
               check.attributes = FALSE, tolerance = .2)
  expect_equal(mean(fixef$sat), 10, tolerance = .02)
  expect_equal(sd(fixef$sat), 2, tolerance = .02)
  
})
