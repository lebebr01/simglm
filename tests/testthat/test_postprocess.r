context("test post process") 

test_that('ifelse post process', {
  set.seed(321) 
  
  sim_args <- list(
    formula = y ~ 1  + time + 
      control + treat_knot + delay_knot + control_post +
      time:treat_knot + time:delay_knot + 
      control:treat_knot + control:time:treat_knot + 
      control_post:delay_knot + control_post:time:delay_knot + (1 + time | id),
    fixed = list(time = list(var_type = 'time', 
                             time_levels = seq(-5, 10, 1)),
                 control = list(var_type = 'factor',
                                levels = c('Treatment 2', 'Treatment 1'), 
                                var_level = 2)),
    knot = list(treat_knot = list(variable = 'time', 
                                  knot_locations = 0),
                delay_knot = list(variable = 'time', 
                                  knot_locations = 4)),
    post = list(control_post = list(variable = 'control', 
                                    fun = 'ifelse',
                                    condition = '== "Treatment 2"',
                                    yes = 1,
                                    no = 0)),
    sample_size = list(level1 = 16, level2 = 150),
    randomeffect = list(int_id = list(variance = 2, var_level = 2),
                        time_id = list(variance = .5, var_level = 2)),
    error = list(variance = 3),
    reg_weights = c(2, .5, 15, 0, 0, 0, 0, 0, 10, 8, 5, 3)
  )
  
  seg_data <- simulate_fixed(data = NULL, sim_args)
  
  expect_setequal(seg_data$control_1, seg_data$control_post)
  expect_setequal(seg_data$control_1 - seg_data$control_post, 
                  c(-1, 1))
  expect_equal(table(seg_data$control_post)[[1]], 
               table(seg_data$control_1)[[2]])
  expect_equal(table(seg_data$control_post)[[2]], 
               table(seg_data$control_1)[[1]])
})