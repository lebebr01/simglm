context("level 3 sim")

test_that('level3 sample size', {
  set.seed(123) 
  
  sim_arguments <- list(
    formula = y ~ 1  + weight + age + treat + econ_state + (1 | id) + (1 | state),
    fixed = list(weight = list(var_type = 'continuous', mean = 180, sd = 30),
                 age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
                 treat = list(var_type = 'factor', 
                              levels = c('Treatment', 'Control'),
                              var_level = 2),
                 econ_state = list(var_type = 'continuous', mean = 50, sd = 15,
                                   var_level = 3)
                 ),
    randomeffect = list(int_id = list(variance = 8, var_level = 2),
                        int_state = list(variance = 5, var_level = 3)
                        ),
    sample_size = list(level1 = 3, level2 = 20, level3 = 50),
    reg_weights = c(1, 0.3, 0.8, 1, 5)
  )
  
  level3_sim <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(level3_sim$state)), 50)
  expect_equal(nrow(level3_sim), 50*20*3)
})