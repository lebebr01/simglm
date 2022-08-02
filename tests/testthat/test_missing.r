context("missing data sim")

test_that('random missing', {
  set.seed(321) 
  sim_arguments <- list(
    formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
    reg_weights = c(4, 0.5, 0.75, 0, 0.33),
    fixed = list(time = list(var_type = 'time'),
                 weight = list(var_type = 'continuous', mean = 180, sd = 30),
                 age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
                 treat = list(var_type = 'factor', 
                              levels = c('Treatment', 'Control'),
                              var_level = 2)),
    randomeffect = list(int_id = list(variance = 8, var_level = 2),
                        time_id = list(variance = 3, var_level = 2)),
    missing_data = list(miss_prop = .25, new_outcome = 'y_missing',
                        type = 'random'),
    sample_size = list(level1 = 50, level2 = 30)
  )
  data_w_missing <- sim_arguments %>%
    simulate_fixed(data = NULL, .) %>%
    simulate_randomeffect(sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments) %>%
    generate_missing(sim_arguments)
  
  expect_type(data_w_missing[['y']], 'double')
  expect_type(data_w_missing[['y_missing']], 'double')
  expect_true(any(is.na(data_w_missing[['y_missing']])))
  expect_false(any(is.na(data_w_missing[['y']])))
  expect_equal(prop.table(table(is.na(data_w_missing[['y_missing']])))[[2]],
               .25, tolerance = .02)
})

test_that('dropout missing', {
  set.seed(321) 
  sim_arguments <- list(
    formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
    reg_weights = c(4, 0.5, 0.75, 0, 0.33),
    fixed = list(time = list(var_type = 'time'),
                 weight = list(var_type = 'continuous', mean = 180, sd = 30),
                 age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
                 treat = list(var_type = 'factor', 
                              levels = c('Treatment', 'Control'),
                              var_level = 2)),
    randomeffect = list(int_id = list(variance = 8, var_level = 2),
                        time_id = list(variance = 3, var_level = 2)),
    missing_data = list(miss_prop = .45, new_outcome = 'missing_y',
                        clust_var = 'id', type = 'dropout'),
    sample_size = list(level1 = 10, level2 = 50)
  )
  data_w_missing <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_randomeffect(sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments) |>
    generate_missing(sim_arguments)
  
  expect_type(data_w_missing[['y']], 'double')
  expect_type(data_w_missing[['missing_y']], 'double')
  expect_true(any(is.na(data_w_missing[['missing_y']])))
  expect_false(any(is.na(data_w_missing[['y']])))
  expect_equal(prop.table(table(is.na(data_w_missing[['missing_y']])))[[2]],
               .226, tolerance = .02)
  
  prop_missing <- prop.table(table(is.na(data_w_missing[['missing_y']]), data_w_missing[['time']]))[2, ]
  expect_lte(prop_missing[[3]], prop_missing[[4]])
  expect_lte(prop_missing[[5]], prop_missing[[6]])
})

test_that('dropout by location', {
  set.seed(321) 
  sim_arguments <- list(
    formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
    reg_weights = c(4, 0.5, 0.75, 0, 0.33),
    fixed = list(time = list(var_type = 'time'),
                 weight = list(var_type = 'continuous', mean = 180, sd = 30),
                 age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
                 treat = list(var_type = 'factor', 
                              levels = c('Treatment', 'Control'),
                              var_level = 2)),
    randomeffect = list(int_id = list(variance = 8, var_level = 2),
                        time_id = list(variance = 3, var_level = 2)),
    missing_data = list(new_outcome = 'y_missing',
                        dropout_location = c(3, 9, 1, 6, 7, 8, 6, 9, 2, 4, 6, 5, 8, 9, 4, 5, 
                                             6, 7, 2, 9),
                        clust_var = 'id', type = 'dropout'),
    sample_size = list(level1 = 10, level2 = 20)
  )
  data_w_missing <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_randomeffect(sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments) |>
    generate_missing(sim_arguments)
  
  expect_type(data_w_missing[['y']], 'double')
  expect_type(data_w_missing[['y_missing']], 'double')
  expect_true(any(is.na(data_w_missing[['y_missing']])))
  expect_false(any(is.na(data_w_missing[['y']])))
  
  expect_true(is.na(subset(data_w_missing, id == 1 & time == 2, select = y_missing)[[1]]))
  expect_false(is.na(subset(data_w_missing, id == 1 & time == 1, select = y_missing)[[1]]))
  expect_true(is.na(subset(data_w_missing, id == 2 & time == 8, select = y_missing)[[1]]))
  expect_false(is.na(subset(data_w_missing, id == 2 & time == 7, select = y_missing)[[1]]))
})

test_that("missing at random", {
  set.seed(321) 
  sim_arguments <- list(
    formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
    reg_weights = c(4, 0.5, 0.75, 0, 0.33),
    fixed = list(time = list(var_type = 'time'),
                 weight = list(var_type = 'continuous', mean = 180, sd = 30,
                               var_level = 1),
                 age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
                 treat = list(var_type = 'factor', 
                              levels = c('Treatment', 'Control'),
                              var_level = 2)),
    randomeffect = list(int_id = list(variance = 8, var_level = 2),
                        time_id = list(variance = 3, var_level = 2)),
    missing_data = list(new_outcome = 'y_missing', miss_cov = 'weight', 
                        mar_prop = seq(from = 0, to = .9, length.out = 200),
                        type = 'mar'),
    sample_size = list(level1 = 10, level2 = 20)
  )
  data_w_missing <- sim_arguments %>%
    simulate_fixed(data = NULL, .) %>%
    simulate_randomeffect(sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments) %>%
    generate_missing(sim_arguments)
  
  expect_type(data_w_missing[['y']], 'double')
  expect_type(data_w_missing[['y_missing']], 'double')
  expect_true(any(is.na(data_w_missing[['y_missing']])))
  expect_false(any(is.na(data_w_missing[['y']])))
  
  prop_missing <- table(is.na(data_w_missing[['y_missing']]), cut(data_w_missing[['weight']], breaks = 10))[2,] / 
    table(cut(data_w_missing[['weight']], breaks = 10))
  expect_lte(prop_missing[[4]], prop_missing[[5]])
  expect_lte(prop_missing[[7]], prop_missing[[8]])
})

test_that("mar small", {
  set.seed(321) 
  
  sim_arguments <- list(
    formula = y ~ 1 + age + treat,
    reg_weights = c(0.75, 0, 0.33, 0.6),
    fixed = list(age = list(var_type = 'ordinal', levels = 30:60),
                 treat = list(var_type = 'factor', 
                              levels = c('Control', 'Low Dose', 'High Dose'))),
    missing_data = list(new_outcome = 'y_missing', miss_cov = 'treat', 
                        mar_prop = c(0.1, 0.2, 0.4),
                        type = 'mar'),
    sample_size = 10000
  )
  
  simulated_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments) |>
    generate_missing(sim_arguments)
  
  prop_missing <- addmargins(table(simulated_data$treat, 
                                   is.na(simulated_data$y_missing)))[, 2] / 
    addmargins(table(simulated_data$treat, 
                     is.na(simulated_data$y_missing)))[, 3]
  
  expect_equal(as.numeric(round(prop_missing[1:3], 2)), c(0.1, 0.2, 0.4), 
               tolerance = 1, )
    
})