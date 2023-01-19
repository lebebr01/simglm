context("oridinal outcome simulation")

test_that("ordinal data simulation", {
  set.seed(2023)
  
  sim_arguments <- list(
    formula = y ~ 1 + turnover,
    fixed = list(turnover = list(var_type = 'continuous', mean = 10, sd = 3)),
    error = list(variance = 0.1),
    sample_size = 1000,
    reg_weights = list(
      c(-2, 0.2),
      c(-5, 0.5)
    ),
    outcome_type = 'multinomial'
  )
  
  tmp_data <- simulate_fixed(data = NULL, sim_arguments) |> 
    simulate_error(sim_arguments) |> 
    generate_response(sim_arguments)
  
  expect_equal(1000, nrow(tmp_data))
  expect_equal(3, length(unique(tmp_data$y)))
  expect_equal(all(is.na(tmp_data$outcome_category)), TRUE)
  
  set.seed(2023)
  
  sim_arguments <- list(
    formula = y ~ 1 + turnover,
    fixed = list(turnover = list(var_type = 'continuous', mean = 10, sd = 3)),
    error = list(variance = 0.1),
    sample_size = 1000,
    reg_weights = list(
      c(-2, 0.2),
      c(-5, 0.5)
    ),
    outcome_type = 'multinomial',
    multinomial_categories = c('Minnesota', 'Wisconsin', 'Illinois')
  )
  
  tmp_data <- simulate_fixed(data = NULL, sim_arguments) |> 
    simulate_error(sim_arguments) |> 
    generate_response(sim_arguments)
  
  expect_equal(1000, nrow(tmp_data))
  expect_equal(3, length(unique(tmp_data$y)))
  expect_equal(all(is.na(tmp_data$outcome_category)), TRUE)
  
})