context('ranef_error')

test_that('correct length', {
  expect_equal(length(sim_rand_eff(random_var = 3, n = 25, rand_gen = 'rnorm')), 
               25)
})

test_that('var random_var', {
  expect_equal(as.numeric(var(sim_rand_eff(random_var = 3, n = 100000,
                                  rand_gen = 'rnorm', ther_sim = TRUE))),
               expected = 3,
               tolerance = .05)
})

test_that('skew', {
  expect_gt(e1071::skewness(sim_rand_eff(random_var = 3, n = 500, 
                                         rand_gen = 'rchisq', df = 1)),
            0)
  expect_gt(e1071::kurtosis(sim_rand_eff(random_var = 3, n = 500, 
                                         rand_gen = 'rt', df = 3)),
            0)
})
