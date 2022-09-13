context("simulate power")

test_that('compute_statistics dimensions', {
  set.seed(321) 
  
  sim_arguments <- list(
    formula = y ~ 1 + weight + age + sex,
    fixed = list(weight = list(var_type = 'continuous', mean = 180, sd = 30),
                 age = list(var_type = 'ordinal', levels = 30:60),
                 sex = list(var_type = 'factor', levels = c('male', 'female'))),
    error = list(variance = 25),
    sample_size = 10,
    reg_weights = c(2, 0.3, -0.1, 0.5),
    model_fit = list(formula = y ~ 1 + age + sex,
                     model_function = 'lm',
                     reg_weights_model = c(2, -0.1, 0.5)),
    replications = 10,
    extract_coefficients = TRUE
  )
  
  expect_equal(nrow(replicate_simulation(sim_arguments) %>%
    compute_statistics(sim_arguments)), 3)
  expect_equal(ncol(replicate_simulation(sim_arguments) %>%
                      compute_statistics(sim_arguments)), 12)
  expect_equal(ncol(replicate_simulation(sim_arguments) %>%
                      compute_statistics(sim_arguments, power = FALSE)), 9)
  expect_equal(ncol(replicate_simulation(sim_arguments) %>%
                      compute_statistics(sim_arguments, type_1_error = FALSE)), 9)
  expect_equal(ncol(replicate_simulation(sim_arguments) %>%
                      compute_statistics(sim_arguments, precision = FALSE)), 9)
  expect_equal(ncol(replicate_simulation(sim_arguments) %>%
                      compute_statistics(sim_arguments,
                                         power = FALSE, 
                                         type_1_error = FALSE,
                                         precision = FALSE)), 3)
})

