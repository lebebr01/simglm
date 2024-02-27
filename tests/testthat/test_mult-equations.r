context("multiple equations")

test_that('mutiple equations different outcome', {
  set.seed(1212) 
  
  sim_arguments <- list(
    formula = list(
      symp_post ~ symp_pre + treatment,
      symp_followup ~ symp_pre + treatment
    ),
    fixed = list(
      symp_pre = list(var_type = 'continuous', 
                      mean = 0, sd = 1),
      treatment = list(var_type = 'factor', 
                       levels = c('control', 'treatment'))
    ),
    error = list(variance = 1),
    sample_size = 100,
    reg_weights = list(
      c(0, -0.5, -0.25),
      c(0, -0.5, 50)
    )
  )
  
  symp_data <- simulate_fixed(data = NULL, sim_arguments) |> 
    simulate_error(sim_arguments) |> 
    generate_response(sim_arguments)
  
  expect_equal(ncol(symp_data), 10)
  expect_equal(nrow(symp_data), 100)
  expect_equal(mean(symp_data[symp_data$treatment == 'control', 'symp_post']),
               mean(symp_data[symp_data$treatment == 'control', 'symp_followup']))
  expect_gt(mean(symp_data[symp_data$treatment == 'treatment', 'symp_followup']),
            mean(symp_data[symp_data$treatment == 'treatment', 'symp_post']))
  expect_gt(mean(symp_data[symp_data$treatment == 'treatment', 'symp_followup']),
            mean(symp_data[symp_data$treatment == 'control', 'symp_followup']))
  expect_lt(mean(symp_data[symp_data$treatment == 'treatment', 'symp_post']),
            mean(symp_data[symp_data$treatment == 'control', 'symp_post']))
  
})

