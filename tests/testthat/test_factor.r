# context('valid factor simulation')
# 
# test_that('factor simulation runs', {
#   fixed <- ~ 1 + day.o + shift.o + treat.o 
#   fixed_param <- c(2, 4, 1, 3.5)
#   cov_param <- NULL
#   fact_vars <- list(numlevels = c(7, 3, 3), var_type = c('single', 'single', "single"),
#                     opts = list(list(replace = TRUE), list(replace = TRUE),
#                                 list(replace = TRUE)))
#   n <- 150
#   error_var <- 3
#   with_err_gen = 'rnorm'
#   temp_single <-  sim_reg(fixed = fixed,  fixed_param = fixed_param,
#                           cov_param = cov_param, n = n, error_var = error_var,
#                           with_err_gen = with_err_gen,  data_str = "single", 
#                           fact_vars = fact_vars)
#   expect_equal(length(table(temp_single$day.o)), 7)
#   expect_equal(nrow(temp_single), 150)
# })
# 
# # test_that('specify different probs', {
#   # fixed <- ~ 1 + day.o + shift.o + treat.o 
#   # fixed_param <- c(2, 4, 1, 3.5)
#   # cov_param <- NULL
#   # fact_vars <- list(numlevels = c(7, 3, 3), var_type = c('single', 'single', "single"),
#   #                   probs = list(NULL, c(.3, .5, .2), NULL))
#   # n <- 150
#   # error_var <- 3
#   # with_err_gen = 'rnorm'
#   # temp_single <-  sim_reg(fixed = fixed,  fixed_param = fixed_param,
#   #                         cov_param = cov_param, n = n, error_var = error_var,
#   #                         with_err_gen = with_err_gen,  data_str = "single", 
#   #                         fact_vars = fact_vars)
#   # expect_equal(length(table(temp_single$day.o)), 7)
#   # expect_equal(nrow(temp_single), 150)
# # })
# 
# test_that('error for incorrect level specification', {
#   fixed <- ~ 1 + day.o + shift.o + treat.o 
#   fixed_param <- c(2, 4, 1, 3.5)
#   cov_param <- NULL
#   fact_vars <- list(numlevels = c(7, 3, 3), var_type = c('lvl1', 'single', "single"),
#                     opts = list(list(replace = TRUE), list(replace = TRUE),
#                                 list(replace = TRUE)))
#   n <- 150
#   error_var <- 3
#   with_err_gen = 'rnorm'
#   
#   expect_error(sim_reg(fixed = fixed,  fixed_param = fixed_param,
#                           cov_param = cov_param, n = n, error_var = error_var,
#                           with_err_gen = with_err_gen,  data_str = "single", 
#                           fact_vars = fact_vars))
# })
