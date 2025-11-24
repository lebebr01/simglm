context("Complex Interaction simulation")

test_that('3way interaction', {
  set.seed(1985)
  sim_arguments <- list(
    formula = y ~ 1 + sex + type + education + sex:type,
    fixed = list(
      sex = list(var_type = 'factor', levels = c('fem', 'male')),
      type = list(var_type = 'factor', levels = c('type1', 'type2', 'type3')),
      education = list(
        var_type = 'factor',
        levels = c('freshman', 'sophomore', 'junior', 'senior')
      )
    ),
    error = list(variance = 0.2),
    sample_size = 150,
    reg_weights = c(0, 0, 1, 2, 0, 0) ## less parameters than previous example!
    # outcome_type = 'count'
  )
  fixed_vars <- simulate_fixed(data = NULL, sim_args = sim_arguments)

  expect_equal(ncol(fixed_vars), 13)
  expect_equal(length(unique(fixed_vars[['education']])), 4)
  expect_equal(length(unique(fixed_vars[['education_1']])), 2)
  expect_equal(length(unique(fixed_vars[['education_2']])), 2)
  expect_equal(length(unique(fixed_vars[['education_3']])), 2)

  sim_arguments <- list(
    formula = y ~ 1 +
      sex +
      type +
      education +
      sex:type +
      sex:education +
      education:type +
      sex:education:type,
    fixed = list(
      sex = list(var_type = 'factor', levels = c('fem', 'male')),
      type = list(var_type = 'factor', levels = c('type1', 'type2', 'type3')),
      education = list(
        var_type = 'factor',
        levels = c('freshman', 'sophomore', 'junior', 'senior')
      )
    ),
    error = list(variance = 0.2),
    sample_size = 150,
    reg_weights = c(0, 0, 1, 2, 0, 0) ## less parameters than previous example!
    # outcome_type = 'count'
  )
  fixed_vars <- simulate_fixed(data = NULL, sim_args = sim_arguments)

  expect_equal(ncol(fixed_vars), 28)
  expect_equal(length(unique(fixed_vars[['education']])), 4)
  expect_equal(length(unique(fixed_vars[['education_1']])), 2)
  expect_equal(length(unique(fixed_vars[['education_2']])), 2)
  expect_equal(length(unique(fixed_vars[['education_3']])), 2)

  sim_arguments <- list(
    formula = y ~ 1 +
      sex +
      type +
      act +
      sex:type +
      sex:act +
      type:act +
      sex:act:type,
    fixed = list(
      sex = list(var_type = 'factor', levels = c('fem', 'male')),
      type = list(var_type = 'factor', levels = c('type1', 'type2', 'type3')),
      act = list(var_type = 'ordinal', levels = 15:36)
    ),
    error = list(variance = 0.2),
    sample_size = 150,
    reg_weights = c(0, 0, 1, 2, 0, 0) ## less parameters than previous example!
    # outcome_type = 'count'
  )
  fixed_vars <- simulate_fixed(data = NULL, sim_args = sim_arguments)

  expect_equal(ncol(fixed_vars), 15)
  expect_type(fixed_vars[['act.sex_1.type_1']], 'double')
  expect_type(fixed_vars[['act.sex_1.type_2']], 'double')
  expect_type(fixed_vars[['type']], 'integer')

  sim_args <- list(
    formula = y ~ 1 +
      time +
      control +
      treat_knot +
      time:treat_knot +
      control:treat_knot +
      (1 + time | id),
    fixed = list(
      time = list(var_type = 'time', time_levels = seq(-5, 10, 1)),
      control = list(
        var_type = 'factor',
        levels = c('Control', 'Treatment'),
        var_level = 2
      )
    ),
    knot = list(treat_knot = list(variable = 'time', knot_locations = 0)),
    sample_size = list(level1 = 16, level2 = 50),
    randomeffect = list(
      int_id = list(variance = 5, var_level = 2),
      time_id = list(variance = 1.5, var_level = 2)
    ),
    error = list(variance = 10),
    reg_weights = c(2, 0.5, 3, -3, 0, -0.15)
  )

  fixed_vars <- simulate_fixed(data = NULL, sim_args)

  expect_equal(ncol(fixed_vars), 9)
  expect_type(fixed_vars[['time.treat_knot']], 'double')
  expect_type(fixed_vars[['treat_knot.control_1']], 'double')
  expect_equal(length(unique(fixed_vars[['treat_knot.control_1']])), 2)
  expect_equal(length(unique(fixed_vars[['time.treat_knot']])), 11)
})
