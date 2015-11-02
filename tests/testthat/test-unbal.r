context("unbal")

test_that("return error",{
  fixed <- ~1 + diff + act + diff:act
  random <- ~1 +  diff
  fixed.param <- c(4, 6, 2.3, 7)
  random_param <- list(random.param = c(7, 2), rand_gen = 'rnorm')
  cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
  n <- 150
  p <- 30
  unbal <- TRUE
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "cross"
  unbalCont <- NULL
  expect_error(sim_reg(fixed = fixed, random = random, fixed.param = fixed.param, 
                       random_param = random_param, cov.param = cov.param, k = NULL, 
                       n = n, p = NULL, error_var = error_var,
                       with_err_gen = with_err_gen, data_str = data_str, unbal = TRUE, 
                       unbalCont = unbalCont),
               "Must specify unbalCont when unbal = TRUE")
})
