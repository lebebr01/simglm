context('test generated data') 

test_that('generated continuous data', {
  sim_arguments <- list(
    formula = y ~ 1 + sex + type + sex:type,
    fixed = list(sex = list(var_type = 'factor', levels=c('fem','male')),
                 type = list(var_type = 'factor', levels = c('type1', 'type2', 'type3'))),
    error = list(variance = 0.2),
    sample_size = 150,
    reg_weights = c(0,0,1,2,0, 0)    ## less parameters than previous example!
    # outcome_type = 'count'
  )
  
  sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments)
  
  expect_equal(nrow(sim_data), 150)
  expect_equal(ncol(sim_data), 13)
  
})

test_that('generated poisson data', {
  sim_arguments <- list(
    formula = y ~ 1 + turnover + type + turnover:type,
    fixed = list(turnover = list(var_type = 'continuous', mean = 10, sd = 3),
                 type = list(var_type = 'factor', levels = c('A','B','C'), prob = c(.2,.6,.2))),
    error = list(variance = 0.1),
    sample_size = 100,
    reg_weights = c(2, 0.01, 0.5, -0.25, -1, 0.1),
    outcome_type = 'poisson'
  )
  
  
  sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments)
  
  expect_equal(nrow(sim_data), 100)
  expect_equal(ncol(sim_data), 13)
})