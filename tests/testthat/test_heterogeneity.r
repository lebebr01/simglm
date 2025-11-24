context('heterogeneity')

test_that('heterogeneity of variance', {
  set.seed(2021)

  simulation_arguments <- list(
    formula = y ~ 1 + group,
    fixed = list(
      group = list(var_type = 'factor', levels = c('male', 'female'))
    ),
    sample_size = 500,
    error = list(variance = 1),
    heterogeneity = list(variable = 'group', variance = c(1, 8)),
    reg_weights = c(0, 0.15)
  )

  h_data <- simulate_fixed(data = NULL, simulation_arguments) |>
    simulate_error(simulation_arguments) |>
    simulate_heterogeneity(simulation_arguments)

  h_var <- h_data |>
    group_by(group) |>
    summarise(var_error = var(error), var_o_error = var(orig_error))

  expect_equal(h_var[['var_error']][1], h_var[['var_o_error']][1])
  expect_gt(h_var[['var_error']][2], h_var[['var_o_error']][2])
})
