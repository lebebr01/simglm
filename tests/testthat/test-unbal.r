context("unbal")

test_that("return error",{
  fixed <- ~1 + diff + act + diff:act
  random <- ~1 +  diff
  fixed_param <- c(4, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
                    var_type = c("lvl1", "lvl2"),
                    opts = list(list(mean = 0, sd = 1.5), 
                                list(mean = 0, sd = 4)))
  n <- 150
  p <- 30
  unbal <- TRUE
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "cross"
  unbalCont <- NULL
  expect_error(sim_reg(fixed = fixed, random = random, fixed_param = fixed_param, 
                       random_param = random_param, cov_param = cov_param, k = NULL, 
                       n = n, p = NULL, error_var = error_var,
                       with_err_gen = with_err_gen, data_str = data_str, unbal = TRUE, 
                       unbalCont = unbalCont),
               "Must specify unbalCont when unbal = TRUE")
})
