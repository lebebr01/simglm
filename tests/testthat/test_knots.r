# context('knot_sim')
# 
# test_that('fixef knots', {
#   set.seed(100)
#   fixed <- ~ 1 + act + gpa + act_k
#   fixed_vars <- attr(terms(fixed),"term.labels")
#   n <- 10000
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
#                     var_type = c("single", "single"),
#                     opts = list(list(mean = 0, sd = 4),
#                                 list(mean = 0, sd = 3)))
#   knot_args <- list(var = 'act', knot_locations = -1)
#   
#   fixef <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars = NULL, 
#                             knot_args = knot_args)
#   
#   expect_equal(table(fixef$Omat$act_k), table(fixef$Omat$act > -1), 
#                check.attributes = FALSE)
#   
#   # knot_args <- list(var = 'act', knot_locations = -1, right = FALSE)
#   # 
#   # fixef <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars = NULL, 
#   #                           knot_args = knot_args)
#   # 
#   # expect_equal(table(fixef$Omat$act_k)[2], table(fixef$Omat$act > -1), 
#   #              check.attributes = FALSE)
#   
#   set.seed(100)
#   fixed <- ~ 1 + act + gpa + gpa.k
#   fixed_vars <- attr(terms(fixed),"term.labels")
#   n <- 5000
#   p <- rep(10, 5000)
#   data_str <- 'cross'
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
#                     var_type = c("level1", "level2"),
#                     opts = list(list(mean = 0, sd = 4), 
#                                 list(mean = 0, sd = 3)))
#   knot_args <- list(var = 'gpa', knot_locations = 0.5)
#   
#   fixef <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, p, data_str, 
#                             cor_vars = NULL, knot_args = knot_args)
#   
#   expect_equal(table(fixef$Omat$gpa.k), table(fixef$Omat$gpa > 0.5), 
#                check.attributes = FALSE)
# })
# 
# test_that('interupt TS', {
#   fixed <- ~1 + time + diff + act + time_k + time:act 
#   random <- ~1 + time + diff
#   fixed_param <- c(4, 2, 6, 2.3, 1, 7)
#   random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
#                     var_type = c("level1", "level2"),
#                     opts = list(list(mean = 0, sd = 1.5), 
#                                 list(mean = 0, sd = 4)))
#   n <- 150
#   p <- 8
#   error_var <- 4
#   with_err_gen <- 'rnorm'
#   data_str <- "long"
#   
#   knot_args = list(var = 'time', knot_locations = 3)
#   
#   temp_long <- sim_reg(fixed, random, random3 = NULL, fixed_param, 
#                        random_param, random_param3 = NULL,
#                        cov_param, k = NULL, n, p, error_var, with_err_gen, 
#                        data_str = data_str, knot_args = knot_args)
#   
#   expect_equal(table(temp_long$time_k), table(temp_long$time >= 3), 
#                check.attributes = FALSE)
#   
#   fixed <- ~1 + time + diff + act + trt_f + time_k + time:act + trt_f:time_k
#   random <- ~1 + time + diff
#   fixed_param <- c(4, 2, 6, 2.3, 1, 0, 1, 0.5)
#   random_param <- list(random_var = c(1, 1, 1), rand_gen = 'rnorm')
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
#                     var_type = c("level1", "level2"),
#                     opts = list(list(mean = 0, sd = 1.5), 
#                                 list(mean = 0, sd = 4)))
#   fact_vars <- list(numlevels = 2, var_type = 'level2', 
#                     opts = list(list(replace = TRUE)))
#   n <- 150
#   p <- 8
#   error_var <- 1
#   with_err_gen <- 'rnorm'
#   data_str <- "long"
#   
#   knot_args = list(var = 'time', knot_locations = 3)
#   
#   temp_long <- sim_reg(fixed, random, random3 = NULL, fixed_param, 
#                        random_param, random_param3 = NULL,
#                        cov_param, k = NULL, n, p, error_var, with_err_gen, 
#                        data_str = data_str, knot_args = knot_args,
#                        fact_vars = fact_vars)
#   
#   expect_equal(table(temp_long$time_k), table(temp_long$time >= 3), 
#                check.attributes = FALSE)
# })
