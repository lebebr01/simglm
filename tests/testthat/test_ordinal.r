context("oridinal outcome simulation")

test_that("ordinal data simulation", {
  set.seed(2022) 
  
  sim_arguments <- list(
    formula = y ~ 1 + turnover,
    fixed = list(turnover = list(var_type = 'continuous', mean = 10, sd = 3)),
    error = list(variance = 0.1),
    sample_size = 100,
    reg_weights = list(
      c(-2, 0.2),
      c(-5, 0.2)
    ),
    outcome_type = 'ordinal'
  )
  
  tmp_data <- simulate_fixed(data = NULL, sim_arguments) |> 
    simulate_error(sim_arguments) |> 
    generate_response(sim_arguments)
  
})