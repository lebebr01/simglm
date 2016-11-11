context("simulate power")

test_that('correct structure', {
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(mean = c(0, 0, 0), sd = c(2, 2, 1), var_type = c("single", "single", "single"))
  n <- 150
  error_var <- 20
  with_err_gen <- 'rnorm'
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 100
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates)
  
  expect_equal(nrow(power_out), 4)
  expect_equal(ncol(power_out), 6)
  
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(mean = c(0, 0, 0), sd = c(2, 2, 1), var_type = c("single", "single", "single"))
  n <- NULL
  error_var <- NULL
  with_err_gen <- 'rnorm'
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 10
  terms_vary <- list(n = c(20, 40, 60, 80, 100), error_var = c(5, 10, 20))
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, terms_vary = terms_vary)
  
  expect_equal(nrow(power_out), 4*5*3)
  expect_equal(length(table(power_out$n)), 5)
  expect_equal(length(table(power_out$error_var)), 3)
  
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(mean = c(0, 0, 0), sd = c(2, 2, 1), var_type = c("single", "single", "single"))
  n <- NULL
  error_var <- NULL
  with_err_gen <- 'rnorm'
  pow_param <- c('act')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 10
  terms_vary <- list(n = c(20, 40, 60, 80, 100), error_var = c(5, 10, 20))
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, terms_vary = terms_vary)
  
  expect_equal(nrow(power_out), 1*5*3)
})
