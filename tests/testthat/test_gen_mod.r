context("power general model specification")

test_that("geeglm model", {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1.5), 
                                list(mean = 0, sd = 4)))
  n <- 10
  p <- 3
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  general_mod <- "geepack::geeglm(sim_data ~ time + diff + act + time:act, 
  family = gaussian, data = data, corstr = 'ar1', id = clustID)"
  
  power <- sim_pow(fixed, random, random3 = NULL, fixed_param, 
                   random_param, random_param3 = NULL,
                   cov_param, k = NULL, n, p, error_var, with_err_gen, 
                   data_str = data_str, pow_param = pow_param, alpha = alpha,
                   pow_dist = pow_dist, pow_tail = pow_tail, 
                   replicates = replicates, raw_power = FALSE,
                   general_mod = general_mod)
  expect_equal(nrow(power), 3)
  expect_equal(min(power$num_repl), 2)
  
})

