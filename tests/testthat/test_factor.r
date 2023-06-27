context('factor simulation force equal')

test_that('level1', {
  library(simglm)
  set.seed(321) 
  
  sim_arguments <- list(
    formula = y ~ 1 + trt,
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 'treatment'),
                            force_equal = TRUE)),
    sample_size = 10
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 1)
  
  sim_arguments <- list(
    formula = y ~ 1 + trt,
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 'treatment'),
                            force_equal = TRUE)),
    sample_size = 15
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 2)

  sim_arguments <- list(
    formula = y ~ 1 + trt,
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 
                                       'treatment', 
                                       'treatment2', 
                                       'treatment3'),
                            force_equal = TRUE)),
    sample_size = 40
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 1)
  
  set.seed(1)
  
  sim_arguments <- list(
    formula = y ~ 1 + trt,
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 
                                       'treatment', 
                                       'treatment2', 
                                       'treatment3'),
                            force_equal = TRUE)),
    sample_size = 42
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 2)
})

test_that('level2', {
  set.seed(1)
  
  sim_arguments <- list(
    formula = y ~ 1 + trt + hospital + (1 | town),
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 
                                       'treatment'),
                            force_equal = TRUE),
                 hospital = list(var_type = 'factor', 
                                 levels = c('hosp1',
                                            'hosp2'),
                                 var_level = 2,
                                 force_equal = TRUE)),
    sample_size = list(level1 = 15, level2 = 10)
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 1)
  expect_equal(length(unique(table(data$hospital))), 1)
  
  set.seed(1)
  
  sim_arguments <- list(
    formula = y ~ 1 + trt + hospital + (1 | town),
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 
                                       'treatment'),
                            force_equal = TRUE),
                 hospital = list(var_type = 'factor', 
                                 levels = c('hosp1',
                                            'hosp2'),
                                 var_level = 2,
                                 force_equal = TRUE)),
    sample_size = list(level1 = 16, level2 = 11)
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 1)
  expect_equal(length(unique(table(data$hospital))), 2)
})

test_that('level3', {
  set.seed(1)
  
  sim_arguments <- list(
    formula = y ~ 1 + trt + hospital + country + (1 | town) + (1 | universe),
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 
                                       'treatment'),
                            force_equal = TRUE),
                 hospital = list(var_type = 'factor', 
                                 levels = c('hosp1',
                                            'hosp2'),
                                 var_level = 2,
                                 force_equal = TRUE),
                 country = list(var_type = 'factor', 
                                levels = c('country1', 
                                           'country2'),
                                var_level = 3,
                                force_equal = TRUE)),
    sample_size = list(level1 = 16, level2 = 11, 
                       level3 = 10)
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 1)
  expect_equal(length(unique(table(data$hospital))), 1)
  expect_equal(length(unique(table(data$country))), 1)
  
  set.seed(1)
  
  sim_arguments <- list(
    formula = y ~ 1 + trt + hospital + country + (1 | town) + (1 | universe),
    fixed = list(trt = list(var_type = 'factor', 
                            levels = c('control', 
                                       'treatment'),
                            force_equal = TRUE),
                 hospital = list(var_type = 'factor', 
                                 levels = c('hosp1',
                                            'hosp2'),
                                 var_level = 2,
                                 force_equal = TRUE),
                 country = list(var_type = 'factor', 
                                levels = c('country1', 
                                           'country2'),
                                var_level = 3,
                                force_equal = TRUE)),
    sample_size = list(level1 = 16, level2 = 11, 
                       level3 = 11)
  )
  
  data <- simulate_fixed(data = NULL, sim_arguments)
  
  expect_equal(length(unique(table(data$trt))), 1)
  expect_equal(length(unique(table(data$hospital))), 2)
  expect_equal(length(unique(table(data$country))), 2)
})
