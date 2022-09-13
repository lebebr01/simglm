context("correlate fixed and random")

test_that('correlate fixed and random', {
  set.seed(321) 
  
  sim_arguments <- list(formula = y ~ 1 + act + gpa + sat, 
                   fixed = list(act = list(var_type = 'continuous',
                                           mean = 20, 
                                           sd = 4),
                                gpa = list(var_type = 'continuous',
                                           mean = 2, 
                                           sd = .5),
                                sat = list(var_type = 'continuous',
                                           mean = 500, 
                                           sd = 100)),
                   correlate = list(fixed = data.frame(x = c('act', 'act', 'gpa'), 
                                                       y = c('gpa', 'sat', 'sat'), 
                                                       corr = c(0.5, .6, .2))),
                   sample_size = 10000)
  
  sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
    correlate_variables(sim_arguments)
  
  expect_equal(cor(sim_data$act, sim_data$gpa), .5, tolerance = .01)
  expect_equal(cor(sim_data$act, sim_data$sat), .6, tolerance = .01)
  expect_equal(cor(sim_data$gpa, sim_data$sat), .2, tolerance = .01)
  
  expect_equal(sd(sim_data$act), 4, tolerance = .1)
  expect_equal(sd(sim_data$gpa), .5, tolerance = .01)
  expect_equal(sd(sim_data$sat), 100, tolerance = .5)
  
  expect_equal(mean(sim_data$act), 20, tolerance = .1)
  expect_equal(mean(sim_data$gpa), 2, tolerance = .05)
  expect_equal(mean(sim_data$sat), 500, tolerance = .1)
  
  sim_arguments <- list(formula = y ~ 1 + act + gpa + sat + (1 + act | id), 
                   fixed = list(act = list(var_type = 'continuous',
                                           mean = 20, 
                                           sd = 4),
                                gpa = list(var_type = 'continuous',
                                           mean = 2, 
                                           sd = .5),
                                sat = list(var_type = 'continuous',
                                           mean = 500, 
                                           sd = 100)),
                   
                   randomeffect = list(int_id = list(variance = 8, var_level = 2),
                                       act_id = list(variance = 3, var_level = 2)),
                   sample_size = list(level1 = 10, level2 = 10000),
                   correlate = list(random = data.frame(x = 'int_id', y = 'act_id',
                                                        corr = .3))
  )
  
  sim_data <- simulate_randomeffect(data = NULL, sim_arguments) |>
    correlate_variables(sim_arguments) 
  
  expect_equal(cor(sim_data$int_id, sim_data$act_id), .3, tolerance = .025)
  expect_equal(sd(sim_data$int_id), sqrt(8), tolerance = .01)
  expect_equal(sd(sim_data$act_id), sqrt(3), tolerance = .01)
  
  sim_arguments <- list(formula = y ~ 1 + act + gpa + sat + (1 + act | id), 
                   fixed = list(act = list(var_type = 'continuous',
                                           mean = 20, 
                                           sd = 4),
                                gpa = list(var_type = 'continuous',
                                           mean = 2, 
                                           sd = .5),
                                sat = list(var_type = 'continuous',
                                           mean = 500, 
                                           sd = 100)),
                   
                   randomeffect = list(int_id = list(variance = 8, var_level = 2),
                                       act_id = list(variance = 3, var_level = 2)),
                   sample_size = list(level1 = 10, level2 = 10000),
                   correlate = list(fixed = data.frame(x = c('act', 'act', 'gpa'), 
                                                       y = c('gpa', 'sat', 'sat'), 
                                                       corr = c(0.5, .6, .2)),
                                    random = data.frame(x = 'int_id', y = 'act_id',
                                                        corr = .3))
  )
  
  sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_randomeffect(sim_arguments) |>
    correlate_variables(sim_arguments)
  
  expect_equal(cor(sim_data$int_id, sim_data$act_id), .3, tolerance = .025)
  expect_equal(sd(sim_data$int_id), sqrt(8), tolerance = .01)
  expect_equal(sd(sim_data$act_id), sqrt(3), tolerance = .01)
  
  expect_equal(cor(sim_data$act, sim_data$gpa), .5, tolerance = .01)
  expect_equal(cor(sim_data$act, sim_data$sat), .6, tolerance = .01)
  expect_equal(cor(sim_data$gpa, sim_data$sat), .2, tolerance = .01)
  
  expect_equal(sd(sim_data$act), 4, tolerance = .01)
  expect_equal(sd(sim_data$gpa), .5, tolerance = .01)
  expect_equal(sd(sim_data$sat), 100, tolerance = .1)
  
  expect_equal(mean(sim_data$act), 20, tolerance = .01)
  expect_equal(mean(sim_data$gpa), 2, tolerance = .01)
  expect_equal(mean(sim_data$sat), 500, tolerance = .1)
})

test_that('ordinal attribute correlation', {
  
  sim_arguments <- list(
    formula = y ~ 1 + education + politics + religiosity + pers_beliefs + rac_acc + main_acc + rac_acc:main_acc ,
    fixed = list(
      education = list(var_type = 'ordinal', levels = 1:5, prob = c(.01, .29, .35, .17, .18)),
      politics = list(var_type = 'continuous', dist = 'rnorm', mean = 0, sd = 1),
      religiosity = list(var_type = 'continuous', dist = 'rnorm', mean = 0, sd = 1),
      pers_beliefs = list(var_type = 'continuous', dist = 'rnorm', mean = 0, sd = 1),
      rac_acc = list(var_type = 'continuous', dist = 'rnorm', mean = 0, sd = 1),
      main_acc = list(var_type = 'continuous', dist = 'rnorm', mean = 0, sd = 1)
    ),
    correlate = list(fixed = data.frame(x = c(rep('education', 5), rep('politics', 4), rep('religiosity', 3), 
                                              rep('pers_beliefs', 2), 'rac_acc'),
                                        y = c('politics', 'religiosity', 'pers_beliefs', 'rac_acc', 'main_acc',
                                              'religiosity', 'pers_beliefs', 'rac_acc', 'main_acc',
                                              'pers_beliefs', 'rac_acc', 'main_acc',
                                              'rac_acc', 'main_acc', 'main_acc'),
                                        corr = c(rep(0.6, 15)))),
    sample_size = 10000,
    reg_weights = c(0, -0.03, -0.07, -0.002, 0.24, 0.31, 0.19, -0.33)
  )
  
  set.seed(2)
  
  sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
    simulate_error(sim_arguments) |>
    correlate_variables(sim_arguments)
  
  expect_true(min(sim_data$education) == 1)
  expect_true(max(sim_data$education) == 5)
  expect_false(max(sim_data$education_corr) == 5)
  expect_false(min(sim_data$education_corr) == 1)
  
  expect_equal(cor(sim_data$education, sim_data$politics), .6, tolerance = .03)
  expect_equal(cor(sim_data$education, sim_data$religiosity), .6, tolerance = .03)
  expect_equal(cor(sim_data$education, sim_data$pers_beliefs), .6, tolerance = .03)
  expect_equal(cor(sim_data$education, sim_data$rac_acc), .6, tolerance = .03)
  expect_equal(cor(sim_data$education, sim_data$main_acc), .6, tolerance = .03)
})

