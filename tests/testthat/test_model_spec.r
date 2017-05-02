context("simulate power model directly")

test_that("lm model specification", {
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 2),
                                list(mean = 0, sd = 1)))
  n <- 150
  error_var <- 20
  with_err_gen <- 'rnorm'
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  expect_error(sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lm_fit_mod = TRUE), 
               'lm_fit_mod must be a formula to pass to lm')
})

test_that('lme4 model specification', {
  
})

