# context('error')
# 
# test_that("correct length", {
#   expect_equal(length(sim_err_single(error_var = 3, n = 50, 
#                                      with_err_gen = 'rnorm')), 50)
#   expect_equal(length(sim_err_nested(error_var = 3, n = 10, p = 25, 
#                                      with_err_gen = 'rnorm')), 25)
#   expect_equal(length(sim_err_nested(error_var = 3, n = 10, p = c(10, 4, 5, 10), 
#                                      with_err_gen = 'rnorm')), 29)
# })
# 
# test_that('correct structure', {
#   expect_output(str(sim_err_single(error_var = 3, n = 50, with_err_gen = 'rnorm',
#                                arima = TRUE, arima_mod = list(ar = .5))), 
#             'Time-Series')
# })
# 
# test_that('var error_var', {
#   expect_equal(var(sim_err_single(error_var = 3, n = 100000, 
#                                   with_err_gen = 'rnorm')), 3,
#                tolerance = .05)
#   expect_equal(var(sim_err_single(error_var = 3, n = 100000, 
#                                   with_err_gen = 'rnorm', 
#                                   ther_sim = TRUE)), 3,
#                tolerance = .05)
# })
# 
# test_that('skew dist', {
#   expect_gt(e1071::skewness(sim_err_single(error_var = 3, n = 1000,
#                                            with_err_gen = 'rchisq',
#                                            lvl1_err_params = list(df = 1))),
#             0)
#   expect_gt(e1071::kurtosis(sim_err_single(error_var = 3, n = 1000,
#                                            with_err_gen = 'rchisq',
#                                            lvl1_err_params = list(df = 1))),
#             0)
# })
