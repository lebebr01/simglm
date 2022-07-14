context('one replication')

sim_arguments <- list(
  formula = amount ~ 1 + treat,
  fixed = list(
    treat = list(var_type = 'factor', levels = c('Control', 'Treatment'))
  ),
  reg_weights = c(0, 0.5),
  error = list(variance = 2),
  vary_arguments = list(
    sample_size = list(50, 150)
  )
)
expect_length(replicate_simulation(sim_arguments), 2)