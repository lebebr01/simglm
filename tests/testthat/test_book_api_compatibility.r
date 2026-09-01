# These are smoke tests for calls published in the simglm book.  Keep the
# argument names and pipelines close to the book, but use deliberately small
# designs and test contracts rather than random values.

test_that("book GLM pipeline remains compatible", {
  set.seed(2022)

  # Chapters 2 and 4: fixed variables, error, and a binary response.
  sim_arguments <- list(
    formula = cured ~ 1 + age + treatment,
    fixed = list(
      age = list(var_type = "ordinal", levels = 20:30),
      treatment = list(
        var_type = "factor",
        levels = c("0 mg", "10 mg", "25 mg", "50 mg")
      )
    ),
    sample_size = 40,
    reg_weights = c(0.5, -0.01, 0.1, 0.2, 0.25),
    outcome_type = "binary"
  )

  sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments)

  expect_s3_class(sim_data, "data.frame")
  expect_equal(nrow(sim_data), 40)
  expect_true(all(c("age", "treatment", "cured") %in% names(sim_data)))
  expect_true(all(sim_data$cured %in% 0:1))
})

test_that("book multilevel and interrupted-time-series pipeline remains compatible", {
  set.seed(2022)

  # Chapters 3 and 13: time, random effects, knots, and post-processing.
  sim_args <- list(
    formula = y ~ 1 + time + control + treat_knot + control_post +
      time:treat_knot + (1 + time | id),
    fixed = list(
      time = list(var_type = "time", time_levels = -2:2),
      control = list(
        var_type = "factor",
        levels = c("Treatment 2", "Treatment 1"),
        var_level = 2
      )
    ),
    knot = list(
      treat_knot = list(variable = "time", knot_locations = 0)
    ),
    post = list(
      control_post = list(
        variable = "control",
        fun = "ifelse",
        condition = "== 'Treatment 2'",
        yes = 1,
        no = 0
      )
    ),
    sample_size = list(level1 = 5, level2 = 6),
    randomeffect = list(
      int_id = list(variance = 2, var_level = 2),
      time_id = list(variance = 0.5, var_level = 2)
    ),
    error = list(variance = 3),
    reg_weights = c(2, 0.5, 1, -1, 0.25, -0.15)
  )

  sim_data <- simulate_fixed(data = NULL, sim_args) |>
    simulate_randomeffect(sim_args) |>
    simulate_error(sim_args) |>
    generate_response(sim_args)

  expect_s3_class(sim_data, "data.frame")
  expect_equal(nrow(sim_data), 30)
  expect_true(all(c("time", "control", "treat_knot", "control_post", "id", "y") %in%
    names(sim_data)))
})

test_that("book causal-design helpers remain compatible", {
  set.seed(121212)

  # Chapters 11 and 14: correlation, heterogeneous errors, and post variables.
  sim_arguments <- list(
    formula = num_credits ~ 1 + math_score + D_post + math_score:D_post,
    fixed = list(
      math_score = list(var_type = "continuous", mean = -2.78, sd = 3.65)
    ),
    post = list(
      D_post = list(
        variable = "math_score",
        fun = "ifelse",
        condition = "<= 0",
        yes = 1,
        no = 0
      )
    ),
    heterogeneity = list(variable = "D_post", variance = c(1, 2)),
    sample_size = 40,
    reg_weights = c(28, 0.47, -1, -0.15),
    error = list(variance = 5)
  )

  rdd_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_error(sim_arguments) |>
    simulate_heterogeneity(sim_arguments) |>
    generate_response(sim_arguments)

  expect_equal(nrow(rdd_data), 40)
  expect_true(all(c("math_score", "D_post", "num_credits") %in% names(rdd_data)))
  expect_true(all(rdd_data$D_post %in% 0:1))

  correlated_arguments <- list(
    formula = chol_post ~ chol_pre + treatment,
    fixed = list(
      chol_pre = list(var_type = "continuous", mean = 0, sd = 1),
      treatment = list(var_type = "ordinal", levels = 0:1)
    ),
    error = list(variance = 1),
    sample_size = 40,
    reg_weights = c(0, 0.5, -0.25),
    correlate = list(
      fixed = data.frame(x = "chol_pre", y = "treatment", corr = 0.6)
    )
  )

  correlated_data <- simulate_fixed(data = NULL, correlated_arguments) |>
    correlate_variables(correlated_arguments) |>
    simulate_error(correlated_arguments) |>
    generate_response(correlated_arguments)

  expect_equal(nrow(correlated_data), 40)
  expect_true(all(c("chol_pre", "treatment", "chol_post") %in%
    names(correlated_data)))
})

test_that("book model-fitting and power workflow remains compatible", {
  set.seed(321)

  # Chapters 5, 8, and 9: varying arguments, replication, and summaries.
  sim_arguments <- list(
    formula = y ~ 1 + weight + sex,
    fixed = list(
      weight = list(var_type = "continuous", mean = 180, sd = 30),
      sex = list(var_type = "factor", levels = c("male", "female"))
    ),
    error = list(variance = 25),
    reg_weights = c(2, 0.3, 0.5),
    model_fit = list(
      formula = y ~ 1 + weight + sex,
      model_function = "lm"
    ),
    replications = 2,
    extract_coefficients = TRUE,
    vary_arguments = list(sample_size = list(30, 40))
  )

  one_fit <- simulate_fixed(data = NULL, c(sim_arguments, sample_size = 40)) |>
    simulate_error(c(sim_arguments, sample_size = 40)) |>
    generate_response(c(sim_arguments, sample_size = 40)) |>
    model_fit(c(sim_arguments, sample_size = 40)) |>
    extract_coefficients()

  expect_s3_class(one_fit, "data.frame")
  expect_true(all(c("term", "estimate") %in% names(one_fit)))

  simulations <- replicate_simulation(sim_arguments)
  statistics <- compute_statistics(
    simulations,
    sim_args = sim_arguments,
    type_1_error = FALSE
  )

  expect_type(simulations, "list")
  expect_true(all(vapply(simulations, inherits, logical(1), what = "data.frame")))
  expect_s3_class(statistics, "data.frame")
  expect_true("term" %in% names(statistics))
})

test_that("book propensity workflow remains compatible", {
  set.seed(123)

  # Chapter 16: treatment is generated by the nested propensity specification.
  sim_arguments <- list(
    formula = achievement ~ 1 + motivation + trt + age + ses,
    fixed = list(
      motivation = list(var_type = "continuous", mean = 0, sd = 20)
    ),
    sample_size = 40,
    error = list(variance = 10),
    reg_weights = c(50, 0.4, 1.2, 0.1, 0.25),
    propensity = list(
      formula = trt ~ 1 + age + ses,
      fixed = list(
        age = list(var_type = "ordinal", levels = -7:7),
        ses = list(var_type = "continuous", mean = 0, sd = 5)
      ),
      error = list(variance = 5),
      reg_weights = c(2, 0.3, -0.5),
      outcome_type = "binary"
    )
  )

  propensity_data <- simulate_propensity(sim_arguments)
  outcome_data <- simulate_fixed(data = NULL, sim_args = sim_arguments) |>
    simulate_error(sim_args = sim_arguments) |>
    generate_response(sim_arguments)

  expect_s3_class(propensity_data, "data.frame")
  expect_s3_class(outcome_data, "data.frame")
  expect_equal(nrow(outcome_data), 40)
  expect_true(all(c("trt", "age", "ses", "achievement") %in%
    names(outcome_data)))
})
