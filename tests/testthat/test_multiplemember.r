context("test multiple-member") 

test_that('fixed multiple member', {
  set.seed(321) 
  
  sim_arguments <- list(
    formula = y ~ 1 + time + weight + age + treat + (1 + time| id) +
      (1 | neighborhood_id),
    fixed = list(time = list(var_type = 'time'),
                 weight = list(var_type = 'continuous', mean = 180, sd = 30),
                 age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
                 treat = list(var_type = 'factor', 
                              levels = c('Treatment', 'Control'),
                              var_level = 2)),
    randomeffect = list(int_id = list(variance = 8, var_level = 2),
                        time_id = list(variance = 3, var_level = 2),
                        int_nid = list(variance = 5, var_level = 2,
                                       multiple_member = TRUE,
                                       num_ids = 12)),
    sample_size = list(level1 = 10, level2 = 20),
    reg_weights = c(1, 0.3, 0.8, 1, 5)
  )
  
  multiple_member <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(ncol(multiple_member), 8)
  expect_true(is.null(multiple_member[['neighborhood_id']]))
  
  mm_random <- simulate_randomeffect(data = NULL, sim_arguments)
  
  expect_equal(ncol(mm_random), 6)
  expect_false(is.null(mm_random[['neighborhood_id']]))
  expect_equal(length(unique(mm_random[['neighborhood_id']])), 12)
  
  mm_random_fixed <- simulate_randomeffect(data = NULL, sim_arguments) |>
    simulate_fixed(sim_arguments)
  
  expect_equal(ncol(mm_random_fixed), 12)
  
  mm_random_fixed <- simulate_randomeffect(data = NULL, sim_arguments) |>
    simulate_fixed(sim_arguments) |>
    simulate_error(sim_arguments) |>
    generate_response(sim_arguments)
  
  expect_equal(ncol(mm_random_fixed), 16)
  expect_type(mm_random_fixed[['y']], 'double')
})