context('cross_class')

test_that('30 cross clusters', {
  fixed <- ~1 + act
  random <- ~1 
  fixed_param <- c(4, 2)
  random_param <- list(random_var = c(.71), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm'),
                    var_type = c("level1"), 
                    opts = list(list(mean = 0, sd = 1.5)))
  n <- 50
  p <- 30
  error_var <- .3
  data_str <- "cross"
  cross_class_params <- list(num_ids = 30, 
                             random_param = list(random_var = 3.7, 
                                                 rand_gen = 'rnorm'))
  
  temp_cross <- sim_reg(fixed = fixed, random = random, 
                        fixed_param = fixed_param, 
                        random_param = random_param, cov_param = cov_param, 
                        k = NULL, n = n, p = p,
                        error_var = error_var, with_err_gen = "rnorm",
                        data_str = data_str, unbal = list(level2 = FALSE),
                        cross_class_params = cross_class_params)
  
  expect_equal(length(table(temp_cross$clustid_cross)), 30)
  
})



