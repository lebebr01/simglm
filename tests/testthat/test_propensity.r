# ------------------------------------------------------------
# Propensity Score Simulation Tests for simglm
# ------------------------------------------------------------

library(testthat)
library(simglm)

# -------- Shared Base Arguments ---------

base_args <- list(
  formula = y ~ trt + age + ses,
  fixed = list(
    age = list(var_type = 'ordinal', levels = 1:3),
    ses = list(var_type = 'continuous', mean = 0, sd = 1)
  ),
  sample_size = 200,
  error = list(variance = 1),
  reg_weights = c(0, 1, 0.5, 0.5),
  propensity = list(
    formula = trt ~ age + ses,
    fixed = list(
      age = list(var_type = 'ordinal', levels = 1:3),
      ses = list(var_type = 'continuous', mean = 0, sd = 1)
    ),
    sample_size = 200,
    reg_weights = c(0, .5, .5),
    outcome_type = 'binary'
  )
)

# ------------------------------------------------------------
test_that("simulate_propensity returns correct structure", {
  set.seed(123)
  dat <- simulate_propensity(base_args)

  expect_s3_class(dat, "data.frame")
  expect_true(all(c("trt", "age", "ses") %in% names(dat)))
  expect_type(dat$trt, "integer")
})

# ------------------------------------------------------------
test_that("propensity simulation is reproducible", {
  set.seed(999)
  d1 <- simulate_propensity(base_args)

  set.seed(999)
  d2 <- simulate_propensity(base_args)

  expect_equal(d1, d2)
})

# ------------------------------------------------------------
test_that("fit_propensity + model_fit works for covariate and ipw", {
  types <- c("covariate", "ipw")

  for (ptype in types) {
    args <- base_args
    args$propensity_model <- list(
      formula = trt ~ age + ses,
      propensity_type = ptype
    )

    dat <- simulate_fixed(NULL, args) |>
      simulate_error(sim_args = args) |>
      generate_response(args) |>
      fit_propensity(args) |>
      model_fit(args)

    expect_s3_class(dat, "lm")
  }
})

# ------------------------------------------------------------
test_that("IPW weights are not extreme", {
  args <- base_args
  args$propensity_model <- list(
    formula = trt ~ age + ses,
    propensity_type = "ipw"
  )

  dat <- simulate_fixed(NULL, args) |>
    simulate_error(sim_args = args) |>
    generate_response(args) |>
    fit_propensity(args)

  expect_true(all(dat$weights > 0))
  expect_true(median(dat$weights) < 10)
})

# ------------------------------------------------------------
test_that("incorrect propensity formula errors", {
  args_bad <- base_args
  args_bad$propensity$formula <- trt ~ wrongvar

  expect_error(simulate_propensity(args_bad))
})

# ------------------------------------------------------------
test_that("multilevel propensity simulation runs", {
  lvl <- sample(10:15, size = 20, replace = TRUE)

  args <- list(
    formula = y ~ trt + age + (1 | school),
    sample_size = list(level1 = lvl, level2 = 20),
    fixed = list(age = list(var_type = 'ordinal', levels = 1:3)),
    randomeffect = list(int_school = list(variance = 2, var_level = 2)),
    error = list(variance = 1),
    propensity = list(
      formula = trt ~ age + (1 | school),
      fixed = list(age = list(var_type = 'ordinal', levels = 1:3)),
      sample_size = list(level1 = lvl, level2 = 20),
      randomeffect = list(int_school = list(variance = 2, var_level = 2)),
      reg_weights = c(0, .5),
      outcome_type = 'binary',
      outcome_level = 2
    ),
    reg_weights = c(0, 1, 0.5)
  )

  dat <- simulate_fixed(NULL, args) |>
    simulate_error(sim_args = args) |>
    simulate_randomeffect(sim_args = args) |>
    generate_response(args)

  expect_true(all(c("school", "trt") %in% names(dat)))
})

# ------------------------------------------------------------
test_that("replicate_simulation runs with propensity", {
  lvl <- sample(10:15, size = 20, replace = TRUE)

  sim_arguments <- list(
    formula = achievement ~ 1 + motivation + trt + age + ses,
    fixed = list(
      motivation = list(var_type = 'continuous', mean = 0, sd = 20)
    ),
    sample_size = 1000,
    error = list(variance = 10),
    reg_weights = c(50, 0.4, 1.2, 0.1, 0.25),
    propensity = list(
      formula = trt ~ 1 + age + ses,
      fixed = list(
        age = list(var_type = 'ordinal', levels = -7:7),
        ses = list(var_type = 'continuous', mean = 0, sd = 5)
      ),
      sample_size = 1000,
      error = list(variance = 5),
      reg_weights = c(2, 0.3, -0.5),
      outcome_type = 'binary'
    ),
    model_fit = list(
      formula = achievement ~ 1 + motivation + trt + age + ses,
      model_function = 'lm'
    ),
    propensity_model = list(
      formula = trt ~ 1 + age + ses,
      propensity_type = 'covariate'
    ),
    replications = 5,
    extract_coefficients = TRUE
  )
  res <- replicate_simulation(sim_arguments) |>
    compute_statistics(
      sim_arguments,
      alternative_power = FALSE,
      type_1_error = FALSE
    )

  expect_s3_class(res, "data.frame")
  expect_true(any(grepl("trt", res$term)))
})

test_that("replicate_simulation runs with propensity level2", {
  lvl <- sample(10:15, size = 20, replace = TRUE)

  sim_arguments <- list(
    formula = achievement ~ 1 + motivation + trt + age + ses + (1 | school),
    fixed = list(
      motivation = list(var_type = 'continuous', mean = 0, sd = 20)
    ),
    sample_size = list(level1 = lvl, level2 = 20),
    error = list(variance = 10),
    randomeffect = list(int_school = list(variance = 2, var_level = 2)),
    reg_weights = c(50, 0.4, 1.2, 0.1, 0.25),
    propensity = list(
      formula = trt ~ 1 + age + ses + (1 | school),
      fixed = list(
        age = list(var_type = 'ordinal', levels = -7:7),
        ses = list(var_type = 'continuous', mean = 0, sd = 5)
      ),
      randomeffect = list(int_school = list(variance = 2, var_level = 2)),
      sample_size = list(level1 = lvl, level2 = 20),
      error = list(variance = 5),
      reg_weights = c(2, 0.3, -0.5),
      outcome_type = 'binary',
      outcome_level = 2
    ),
    model_fit = list(
      formula = achievement ~ 1 + motivation + trt + age + ses,
      model_function = 'lm'
    ),
    propensity_model = list(
      formula = trt ~ 1 + age + ses,
      propensity_type = 'covariate'
    ),
    replications = 5,
    extract_coefficients = TRUE
  )
  res <- replicate_simulation(sim_arguments) |>
    compute_statistics(
      sim_arguments,
      alternative_power = FALSE,
      type_1_error = FALSE
    )

  expect_s3_class(res, "data.frame")
  expect_true(any(grepl("trt", res$term)))
})
