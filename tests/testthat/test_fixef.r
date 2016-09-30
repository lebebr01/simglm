context('fixef_sim')

test_that('sim_factor are discrete', {
  expect_length(table(sim_factor(n = 100, numlevels = 4, prob = c(.25, .25, .25, .25), 
                          var_type = 'single')), 4)
  expect_length(table(sim_factor(n = 100, p = 3, numlevels = 4, prob = c(.25, .25, .25, .25), 
                                 var_type = 'lvl1')), 4)
  expect_length(table(sim_factor(n = 100, p = 3, k = 30, numlevels = 4, prob = c(.25, .25, .25, .25), 
                                 var_type = 'lvl3')), 4)
  expect_length(table(sim_factor(n = 100, p = 3, numlevels = 4, prob = c(.25, .25, .25, .25), 
                                 var_type = 'lvl2')), 4)
})

test_that('sim_continuous are continuous', {
  expect_length(table(sim_continuous(n = 100, mean = 0, sd = 1, 
                                     var_type = 'single')), 100)
  expect_length(table(sim_continuous(n = 100, p = rep(3, 100), mean = 0, sd = 1, 
                                     var_type = 'lvl1')), 300)
  expect_length(table(sim_continuous(n = 100, p = rep(3, 100), mean = 0, sd = 1, 
                                     var_type = 'lvl2')), 100)
  expect_length(table(sim_continuous(n = 100, p = rep(3, 100), k = 30, mean = 0, sd = 1, 
                                     var_type = 'lvl3')), 30)
})
