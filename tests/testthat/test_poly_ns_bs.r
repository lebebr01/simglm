context('poly, ns, bs, fixed effect sim')

test_that('fixed poly sim', {
  set.seed(321)

  sim_arguments <- list(
    formula = y ~ 1 + x1 + x2,
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 30),
      x2 = list(var_type = 'continuous', mean = 40, sd = 5)
    ),
    sample_size = 10
  )

  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 4)

  sim_arguments <- list(
    formula = y ~ 1 + x1 + poly(x2, degree = 2),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 30),
      x2 = list(var_type = 'continuous', mean = 40, sd = 5)
    ),
    sample_size = 10
  )

  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 6)

  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3) + poly(x2, degree = 2),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 30),
      x2 = list(var_type = 'continuous', mean = 40, sd = 5)
    ),
    sample_size = 10
  )

  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 9)
})

test_that('raw poly', {
  set.seed(312)
  # Raw uncorrelated poly
  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3) + poly(x2, degree = 2),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 1),
      x2 = list(var_type = 'continuous', mean = 40, sd = 1)
    ),
    sample_size = 10
  )

  expect_true(all.equal(
    sapply(simulate_fixed(data = NULL, sim_arguments), mean)[2:6],
    rep(0, 5),
    check.attributes = FALSE
  ))

  # raw = TRUE
  sim_arguments <- list(
    formula = y ~ 1 +
      poly(x1, degree = 3, raw = TRUE) +
      poly(x2, degree = 2, raw = TRUE),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 1),
      x2 = list(var_type = 'continuous', mean = 40, sd = 1)
    ),
    sample_size = 10
  )

  expect_false(isTRUE(all.equal(
    sapply(simulate_fixed(data = NULL, sim_arguments), mean)[2:6],
    rep(0, 5),
    check.attributes = FALSE
  )))
})

test_that('poly outcome', {
  set.seed(312)
  # Raw uncorrelated poly
  sim_arguments <- list(
    formula = y ~ 1 + poly(x1, degree = 3) + poly(x2, degree = 2),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 1),
      x2 = list(var_type = 'continuous', mean = 40, sd = 1)
    ),
    sample_size = 10,
    reg_weights = c(2, 1, 0.5, 0.25, 1, 0.5)
  )

  expect_equal(
    simulate_fixed(data = NULL, sim_arguments) |>
      simulate_error(sim_arguments) |>
      generate_response(sim_arguments) |>
      nrow(),
    10
  )
  expect_equal(
    simulate_fixed(data = NULL, sim_arguments) |>
      simulate_error(sim_arguments) |>
      generate_response(sim_arguments) |>
      ncol(),
    13
  )
})

test_that('ns generation df', {
  library(splines)
  sim_arguments <- list(
    formula = y ~ 1 + x1 + ns(x2, df = 4),
    fixed = list(
      x1 = list(var_type = 'continuous', mean = 180, sd = 30),
      x2 = list(var_type = 'continuous', mean = 40, sd = 5)
    ),
    sample_size = 10
  )

  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 10)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 8)
})

test_that("poly and factors", {
  set.seed(11111)
  sim_arguments <- list(
    formula = y ~ 1 +
      denomination +
      poly(attendance, degree = 2, raw = TRUE) +
      age +
      friends +
      female,
    fixed = list(
      denomination = list(
        var_type = 'factor',
        levels = c('Catholic', 'Jewish', 'Protestant', 'Other'),
        prob = c(0.2732, 0.0126, 0.3053, 0.4089)
      ),
      attendance = list(
        var_type = 'ordinal',
        levels = 0:4,
        prob = c(0.205, 0.170, 0.201, 0.130, 0.294)
      ),
      age = list(var_type = 'continuous', mean = 47.66, sd = 17.15),
      #age          = list(var_type = 'ordinal', levels = 18:93, mean = 47.66),
      friends = list(
        var_type = 'ordinal',
        levels = 0:1,
        prob = c(0.4903, 0.5097)
      ),
      female = list(
        var_type = 'ordinal',
        levels = 0:1,
        prob = c(0.4095, 0.5905)
      )
    ),
    sample_size = 500
  )

  poly_factor_data <- simulate_fixed(data = NULL, sim_arguments)

  expect_equal(ncol(poly_factor_data), 13)
  expect_equal(nrow(poly_factor_data), 500)
  expect_true(any(
    names(poly_factor_data) %in% paste('denomination', 1:3, sep = "_")
  ))
  expect_true(any(
    names(poly_factor_data) %in% paste('attendance', 1:2, sep = "_")
  ))
})
