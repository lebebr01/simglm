context('error')

test_that("correct length", {
  expect_equal(length(sim_err_single(error_var = 3, n = 50, 
                                     with_err_gen = 'rnorm')), 50)
  expect_equal(length(sim_err_nested(error_var = 3, n = 10, p = 25, 
                                     with_err_gen = 'rnorm')), 25)
  expect_equal(length(sim_err_nested(error_var = 3, n = 10, p = c(10, 4, 5, 10), 
                                     with_err_gen = 'rnorm')), 29)
})