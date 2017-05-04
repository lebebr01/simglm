context("simulate power")

test_that('correct structure', {
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 2),
                                list(mean = 0, sd = 1)))
  n <- 150
  error_var <- 20
  with_err_gen <- 'rnorm'
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE)
  
  expect_equal(nrow(power_out), 4)
  expect_equal(ncol(power_out), 6)
  
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = TRUE)
  
  expect_equal(nrow(power_out), 4)
  expect_equal(ncol(power_out), 7)
  
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 2),
                                list(mean = 0, sd = 1)))
  n <- NULL
  error_var <- NULL
  with_err_gen <- 'rnorm'
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  terms_vary <- list(n = c(20, 40, 60, 80, 100), error_var = c(5, 10, 20))
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, terms_vary = terms_vary, 
                       raw_power = FALSE)
  
  expect_equal(nrow(power_out), 4*5*3)
  expect_equal(length(table(power_out$n)), 5)
  expect_equal(length(table(power_out$error_var)), 3)
  
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 2),
                                list(mean = 0, sd = 1)))
  n <- NULL
  error_var <- NULL
  with_err_gen <- 'rnorm'
  pow_param <- c('act')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  terms_vary <- list(n = c(20, 40, 60, 80, 100), error_var = c(5, 10, 20))
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, terms_vary = terms_vary, 
                       raw_power = FALSE)
  
  expect_equal(nrow(power_out), 1*5*3)
})

test_that('sim_glm power', {
  fixed <- ~ 1 + act + diff
  fixed_param <- c(0.1, 0.5, 0.3)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 4)))
  n <- 50
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  
  power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
                           cov_param = cov_param, 
                           n = n, data_str = "single", 
                           pow_param = pow_param, alpha = alpha,
                           pow_dist = pow_dist, pow_tail = pow_tail, 
                           replicates = replicates, raw_power = FALSE)
  expect_equal(nrow(power_out), 3)
  expect_equal(ncol(power_out), 6)
  
  fixed <- ~ 1 + act + diff
  fixed_param <- c(0.1, 0.5, 0.3)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 4)))
  n <- NULL
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  terms_vary = list(n = c(25, 50, 100))
  
  power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
                           cov_param = cov_param, 
                           n = n, data_str = "single", 
                           pow_param = pow_param, alpha = alpha,
                           pow_dist = pow_dist, pow_tail = pow_tail, 
                           replicates = replicates, terms_vary = terms_vary, 
                           raw_power = FALSE)
  expect_equal(nrow(power_out), 3*3)
  expect_equal(ncol(power_out), 7)
})

test_that('two level power continuous', {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time
  fixed_param <- c(0, 0.2, 0.1, 0.3, 0.05)
  random_param <- list(random_var = c(7, 4), rand_gen = "rnorm")
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("level1", "level2"), 
                    opts = list(list(mean = 0, sd = 1),
                                list(mean = 0, sd = 1)))
  n <- 15
  p <- 3
  error_var <- 1
  data_str <- "long"
  pow_param <- c('time', 'diff', 'act')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  power_out <- sim_pow(fixed = fixed, random = random, 
                       fixed_param = fixed_param, 
                       random_param = random_param, cov_param = cov_param, 
                       k = NULL, n = n, p = p,
                       error_var = error_var, with_err_gen = "rnorm",
                       data_str = data_str, unbal = list(level2 = FALSE), 
                       pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE)
  
  expect_equal(nrow(power_out), 3)
})

test_that("three level power continuous", {
  fixed <- ~1 + time + diff + act + actClust + time:act
  random <- ~1 + time 
  random3 <- ~ 1 + time
  fixed_param <- c(4, 2, 6, 2.3, 7, 0)
  random_param <- list(random_var = c(7, 4), rand_gen = 'rnorm')
  random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("level1", "level2", "level3"),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4), 
                                list(mean = 0, sd = 2)))
  k <- 10
  n <- 15
  p <- 5
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  pow_param <- c('time', 'diff', 'act', 'actClust')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  power_out <- sim_pow(fixed = fixed, random = random, random3 = random3,
                       fixed_param = fixed_param, 
                       random_param = random_param, 
                       random_param3 = random_param3, 
                       cov_param = cov_param, 
                       k = k, n = n, p = p,
                       error_var = error_var, with_err_gen = "rnorm",
                       data_str = data_str, 
                       unbal = list(level3 = FALSE, level2 = FALSE), 
                       pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE)
  
  expect_equal(nrow(power_out), 4)
})

test_that('two level power dich', {
  set.seed(200)
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time
  fixed_param <- c(0, 0.2, 0.1, 0.3, 0.05)
  random_param <- list(random_var = c(7, 4), rand_gen = "rnorm")
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1),
                                list(mean = 0, sd = 1)))
  n <- 20
  p <- 8
  data_str <- "long"
  pow_param <- c('time', 'diff', 'act')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  power_out <- sim_pow_glm(fixed = fixed, random = random, 
                       fixed_param = fixed_param, 
                       random_param = random_param, cov_param = cov_param, 
                       k = NULL, n = n, p = p,
                       data_str = data_str, unbal = list(level2 = FALSE), 
                       pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE)
  
  expect_equal(nrow(power_out), 3)
})

test_that("three level power dich", {
  set.seed(200)
  fixed <- ~1  + diff + act + actClust
  random <- ~1 
  random3 <- ~ 1
  fixed_param <- c(0.2, 0.1, 0.3, 0.05)
  random_param <- list(random_var = 7, rand_gen = 'rnorm')
  random_param3 <- list(random_var = 4, rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
                    var_type = c("level1", "level2", "level3"),
                    opts = list(list(mean = 0, sd = 1.5),
                                list(mean = 0, sd = 4),
                                list(mean = 0, sd = 2)))
  k <- 10
  n <- 15
  p <- 6
  data_str <- "cross"
  pow_param <- c('diff', 'act', 'actClust')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  power_out <- sim_pow_glm(fixed = fixed, random = random, random3 = random3,
                       fixed_param = fixed_param, 
                       random_param = random_param, 
                       random_param3 = random_param3, 
                       cov_param = cov_param, 
                       k = k, n = n, p = p,
                       data_str = data_str, 
                       unbal = list(level2 = FALSE, level3 = FALSE),
                       pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE)
  
  expect_equal(nrow(power_out), 3)
})
