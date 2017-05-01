context('test dimensions of data')

test_that('correct length sim_reg', {
  fixed <- ~1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(2, 4, 1, 3.5, 2)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 3),
                                list(mean = 0, sd = 3)))
  n <- 150
  error_var <- 3
  with_err_gen <- 'rnorm'
  temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, 
                         n = n, error_var = error_var, with_err_gen = with_err_gen, data_str = "single")
  expect_equal(nrow(temp_single), 150)
  expect_equal(length(table(temp_single$ID)), 150)
  expect_equal(ncol(temp_single), 9)
  
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4)))
  n <- 150
  p <- 30
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  temp_long <- sim_reg(fixed, random, random3 = NULL, fixed_param, random_param, 
                       random_param3 = NULL,
                       cov_param, k = NULL, n, p, error_var, with_err_gen, 
                       data_str = data_str)
  expect_equal(nrow(temp_long), 150*30)
  expect_equal(length(table(temp_long$withinID)), 30)
  expect_equal(length(table(temp_long$clustID)), 150)
  
  fixed <- ~1 + time + diff + act + actClust + time:act
  random <- ~1 + time + diff
  random3 <- ~ 1 + time
  fixed_param <- c(4, 2, 6, 2.3, 7, 0)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("level1", "level2", "level3"),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4), 
                                list(mean = 0, sd = 2)))
  k <- 10
  n <- 15
  p <- 10
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  temp_three <- sim_reg(fixed, random, random3, fixed_param, random_param, 
                        random_param3, cov_param, k,n, p, error_var, with_err_gen, 
                        data_str = data_str)
  expect_equal(nrow(temp_three), 15*10*10)
  expect_equal(length(table(temp_three$withinID)), 10)
  expect_equal(length(table(temp_three$clustID)), 15*10)
  expect_equal(length(table(temp_three$clust3ID)), 10)
  
  # Direct time specification
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("level1", "level2"),
                    time_var = c(0, 1, 2, 4, 8),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4)))
  n <- 150
  p <- 5
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  temp_long <- sim_reg(fixed, random, random3 = NULL, fixed_param, random_param, 
                       random_param3 = NULL,
                       cov_param, k = NULL, n, p, error_var, with_err_gen, 
                       data_str = data_str)
  expect_equal(nrow(temp_long), 150*5)
  expect_equal(length(table(temp_long$withinID)), 5)
  expect_equal(length(table(temp_long$clustID)), 150)
  expect_equal(unique(temp_long$time), cov_param$time_var)
})

test_that('correct length sim_glm', {
  fixed <- ~1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(2, 4, 1, 3.5, 2)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 3),
                                list(mean = 0, sd = 3)))
  n <- 150
  temp_single <- sim_glm(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, 
                         n = n, data_str = "single")
  expect_equal(nrow(temp_single), 150)
  expect_equal(length(table(temp_single$ID)), 150)
  
  # Longitudinal linear mixed model example
  set.seed(1)
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(.1, .5, .4, .01, .8)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4)))
  n <- 150
  p <- 30
  data_str <- "long"
  temp_long <- sim_glm(fixed, random, random3 = NULL, fixed_param, random_param, random_param3 = NULL,
                       cov_param, k = NULL, n, p, data_str = data_str)
  expect_equal(nrow(temp_long), 150*30)
  expect_equal(length(table(temp_long$withinID)), 30)
  expect_equal(length(table(temp_long$clustID)), 150)
  
  # Three level example
  set.seed(1)
  fixed <- ~1 + time + diff + act + actClust + time:act
  random <- ~1 + time + diff
  random3 <- ~ 1 + time
  fixed_param <- c(.1, .5, .4, .01, .8, .3)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
                    var_type = c("level1", "level2", "level3"),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4),
                                list(mean = 0, sd = 2)))
  k <- 10
  n <- 15
  p <- 10
  data_str <- "long"
  temp_three <- sim_glm(fixed, random, random3, fixed_param, random_param, 
                        random_param3, cov_param, k,n, p, data_str = data_str)
  expect_equal(nrow(temp_three), 15*10*10)
  expect_equal(length(table(temp_three$withinID)), 10)
  expect_equal(length(table(temp_three$clustID)), 15*10)
  expect_equal(length(table(temp_three$clust3ID)), 10)
})
