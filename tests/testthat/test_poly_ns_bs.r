context('poly_ns_bs_sim')

test_that('fixed poly sim', {
  set.seed(321) 
  
  sim_arguments <- list(
    formula = y ~ 1 + x1 + x2,
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 30),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 5)),
    sample_size = 10
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 4)
  
  sim_arguments <- list(
    formula = y ~ 1 + x1 + poly(x2, degree = 2),
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 30),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 5)),
    sample_size = 10
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 5)
  
  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3) + poly(x2, degree = 2),
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 30),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 5)),
    sample_size = 10
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 7)
})

test_that('raw poly', {
  set.seed(312)
  # Raw uncorrelated poly
  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3) + poly(x2, degree = 2),
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 1),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 1)),
    sample_size = 10
  )
  
  expect_true(all.equal(sapply(simulate_fixed(data = NULL, sim_arguments), 
                               mean)[2:6], rep(0, 5), 
                        check.attributes = FALSE))
  
  # raw = TRUE
  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3, raw = TRUE) + poly(x2, degree = 2, raw = TRUE),
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 1),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 1)),
    sample_size = 10
  )
  
  expect_false(isTRUE(all.equal(sapply(simulate_fixed(data = NULL, sim_arguments), 
                               mean)[2:6], rep(0, 5), 
                        check.attributes = FALSE)))
})

test_that('poly outcome', {
  set.seed(312)
  # Raw uncorrelated poly
  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3) + poly(x2, degree = 2),
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 1),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 1)),
    sample_size = 10,
    reg_weights = c(2, 1, 0.5, 0.25, 1, 0.5)
  )
  
  expect_equal(simulate_fixed(data = NULL, sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments) %>% 
      nrow(), 10)
  expect_equal(simulate_fixed(data = NULL, sim_arguments) %>%
                 simulate_error(sim_arguments) %>%
                 generate_response(sim_arguments) %>% 
                 ncol(), 11)
})

test_that('ns generation df', {
library(splines)
  sim_arguments <- list(
    formula = y ~ 1 + x1 + ns(x2, df = 4),
    fixed = list(x1 = list(var_type = 'continuous', mean = 180, sd = 30),
                 x2 = list(var_type = 'continuous', mean = 40, sd = 5)),
    sample_size = 10
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 7)
})