context("simulate power")

test_that('compute_statistics dimensions', {
  set.seed(321)

  sim_arguments_test <- list(
    formula = y ~ 1 + weight + age + sex,
    fixed = list(
      weight = list(var_type = 'continuous', mean = 180, sd = 30),
      age = list(var_type = 'ordinal', levels = 30:60),
      sex = list(var_type = 'factor', levels = c('male', 'female'))
    ),
    error = list(variance = 25),
    sample_size = 10,
    reg_weights = c(2, 0.3, -0.1, 0.5),
    model_fit = list(
      formula = y ~ 1 + age + sex,
      model_function = 'lm',
      reg_weights_model = c(2, -0.1, 0.5)
    ),
    replications = 10,
    extract_coefficients = TRUE
  )

  # expect_equal(nrow(replicate_simulation(sim_arguments) |>
  #   compute_statistics(sim_arguments, alternative_power = FALSE)), 3)
  # expect_equal(ncol(replicate_simulation(sim_arguments) |>
  #                     compute_statistics(sim_arguments, alternative_power = FALSE)), 12)
  # expect_equal(ncol(replicate_simulation(sim_arguments) |>
  #                     compute_statistics(sim_arguments, power = FALSE, alternative_power = FALSE)), 9)
  expect_equal(
    ncol(
      replicate_simulation(sim_arguments_test) |>
        compute_statistics(
          sim_arguments_test,
          type_1_error = FALSE,
          alternative_power = FALSE
        )
    ),
    7
  )
  # expect_equal(ncol(replicate_simulation(sim_arguments) |>
  #                     compute_statistics(sim_arguments, precision = FALSE, alternative_power = FALSE)), 9)
  expect_equal(
    ncol(
      replicate_simulation(sim_arguments_test) |>
        compute_statistics(
          sim_arguments_test,
          alternative_power = FALSE,
          power = FALSE,
          type_1_error = FALSE,
          precision = FALSE
        )
    ),
    3
  )
})

test_that('power statistics support ns terms', {
  set.seed(321)

  sim_arguments_test <- list(
    formula = y ~ 1 + x1 + ns(x2, df = 4),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 0, sd = 1),
      x2 = list(var_type = 'continuous', mean = 0, sd = 1)
    ),
    error = list(variance = 1),
    sample_size = 80,
    reg_weights = list(
      `(Intercept)` = 0,
      x1 = 0.25,
      `ns(x2, df = 4)` = c(0.75, 0.5, 0, 0)
    ),
    model_fit = list(model_function = 'lm'),
    replications = 5,
    extract_coefficients = TRUE
  )

  power_stats <- replicate_simulation(sim_arguments_test) |>
    compute_statistics(
      sim_arguments_test,
      type_1_error = FALSE,
      alternative_power = FALSE
    )

  expect_equal(nrow(power_stats), 6)
  expect_true(all(paste0("ns(x2, df = 4)", 1:4) %in% power_stats$term))
  expect_true(all(c("avg_estimate", "power", "precision_ratio") %in% names(power_stats)))
})
