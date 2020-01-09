# context('heterogeneity')
# 
# test_that('Single level', {
#   fixed <- ~ 1 + act + diff + numCourse + act:numCourse
#   fixed_param <- c(2, 4, 1, 3.5, 2)
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
#                     var_type = c("single", "single", "single"),
#                     cov_param = list(list(mean = 0, sd = 4),
#                                      list(mean = 0, sd = 3),
#                                      list(mean = 0, sd = 3)))
#   n <- 1500
#   error_var <- c(3, 50, 10)
#   with_err_gen = 'rnorm'
#   
#   temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, 
#                          cov_param = cov_param,
#                          n = n, error_var = error_var, 
#                          with_err_gen = with_err_gen, 
#                          data_str = "single", 
#                          homogeneity = FALSE, heterogeneity_var = 'diff')
#   temp_single$group <- cut(temp_single[['diff']], 
#                            length(error_var), labels = FALSE)
#   expect_equal(as.numeric(tapply(temp_single$err, temp_single$group, var)), 
#                error_var, tolerance = 1)
#   
# })
# 
# test_that('longitudinal', {
#   fixed <- ~1 + time + diff + act + time:act
#   random <- ~1 + time + diff
#   fixed_param <- c(4, 2, 6, 2.3, 7)
#   random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
#                     var_type = c("level1", "level2"), 
#                     time_var = c(0, 0.5, 1, 3, 5),
#                     opts = list(list(mean = 0, sd = 1.5), 
#                                 list(mean = 0, sd = 4)))
#   n <- 150
#   p <- 5
#   error_var <- c(4, 16, 32, 32, 32)
#   with_err_gen <- 'rnorm'
#   data_str <- "long"
#   temp_long <- sim_reg(fixed, random, random3 = NULL, fixed_param, 
#                        random_param, random_param3 = NULL,
#                        cov_param, k = NULL, n, p, error_var, with_err_gen, 
#                        data_str = data_str, homogeneity = FALSE, 
#                        heterogeneity_var = 'time')
#   
#   expect_equal(as.numeric(tapply(temp_long$err, temp_long$time, var)), 
#                error_var, tolerance = 1)
# })
# 
# test_that('three level cross', {
#   fixed <- ~1 + diff + act + actClust
#   random <- ~1 + diff
#   random3 <- ~ 1 
#   fixed_param <- c(4, 6, 2.3, 7)
#   random_param <- list(random_var = c(7, 2), rand_gen = 'rnorm')
#   random_param3 <- list(random_var = c(4), rand_gen = 'rnorm')
#   cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
#                     var_type = c("level1", "level2", "level3"),
#                     opts = list(list(mean = 0, sd = 1.5),
#                                 list(mean = 0, sd = 4), 
#                                 list(mean = 0, sd = 2)))
#   k <- 10
#   n <- 15
#   p <- 10
#   error_var <- c(4, 10, 15, 20)
#   with_err_gen <- 'rnorm'
#   data_str <- "cross"
#   temp_three <- sim_reg(fixed, random, random3, fixed_param, random_param, 
#                         random_param3, cov_param, k,n, p, error_var, with_err_gen, 
#                         data_str = data_str, homogeneity = FALSE, 
#                         heterogeneity_var = 'act')
#   
#   temp_three$group <- cut(temp_three[['act']], 
#                            length(error_var), labels = FALSE)
#   expect_equal(as.numeric(tapply(temp_three$err, temp_three$group, var)), 
#                error_var, tolerance = 1)
# })
# 
