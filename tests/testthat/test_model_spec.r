context("simulate power model directly")

test_that("lm model specification", {
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
  expect_error(sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lm_fit_mod = TRUE), 
               'lm_fit_mod must be a formula to pass to lm')
  
  lm_fit_mod <- sim_data ~ 1 + act + diff + numCourse
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lm_fit_mod = lm_fit_mod)
  expect_equal(nrow(power_out), 4)
  
  pow_param <- c('(Intercept)', 'act')
  lm_fit_mod <- sim_data ~ 1 + act 
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lm_fit_mod = lm_fit_mod)
  expect_equal(nrow(power_out), 2)
  
  pow_param <- c('(Intercept)', 'act', 'diff')
  lm_fit_mod <- sim_data ~ 1 + act 
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lm_fit_mod = lm_fit_mod)
  
  expect_equal(is.na(dplyr::filter(power_out, var == 'diff')$power), TRUE)
  
})

test_that('glm model specification', {
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", "single"),
                    opts = list(list(mean = 0, sd = 2),
                                list(mean = 0, sd = 2),
                                list(mean = 0, sd = 1)))
  n <- 150
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  expect_error(sim_pow_glm(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       glm_fit_mod = TRUE),   
               'glm_fit_mod must be a formula to pass to glm')
  
  glm_fit_mod <- sim_data ~ 1 + act + diff
  glm_fit_family <- binomial
  
  power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                           n = n, data_str = "single", pow_param = pow_param, alpha = alpha,
                           pow_dist = pow_dist, pow_tail = pow_tail, 
                           replicates = replicates, raw_power = FALSE, 
                           glm_fit_mod = glm_fit_mod, glm_fit_family = glm_fit_family)
  
  expect_equal(is.na(dplyr::filter(power_out, var == 'numCourse')$power), TRUE)
  
})

test_that('lme4 model specification', {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1.5), 
                                list(mean = 0, sd = 4)))
  n <- 10
  p <- 3
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  expect_error(sim_pow(fixed, random, random3 = NULL, fixed_param, 
                       random_param, random_param3 = NULL,
                       cov_param, k = NULL, n, p, error_var, with_err_gen, 
                       data_str = data_str, pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lme4_fit_mod = TRUE), 
    'lme4_fit_mod must be a formula to pass to lmer')
  
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
  expect_error(sim_pow(fixed, random, random3, fixed_param, random_param, 
                        random_param3, cov_param, k,n, p, error_var, with_err_gen, 
                        data_str = data_str, pow_param = pow_param, alpha = alpha,
                        pow_dist = pow_dist, pow_tail = pow_tail, 
                        replicates = replicates, raw_power = FALSE, 
                        lme4_fit_mod = TRUE), 
  'lme4_fit_mod must be a formula to pass to lmer')
})

test_that('lme4 glm model specification', {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1.5), 
                                list(mean = 0, sd = 4)))
  n <- 10
  p <- 3
  data_str <- "long"
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  expect_error(sim_pow_glm(fixed, random, random3 = NULL, fixed_param, 
                       random_param, random_param3 = NULL,
                       cov_param, k = NULL, n, p, 
                       data_str = data_str, pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       lme4_fit_mod = TRUE),
               'lme4_fit_mod must be a formula to pass to glmer')
})

test_that('nlme models', {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'), 
                    var_type = c("level1", "level2"),
                    opts = list(list(mean = 0, sd = 1.5), 
                                list(mean = 0, sd = 4)))
  n <- 50
  p <- 6
  error_var <- 4
  with_err_gen <- 'rnorm'
  data_str <- "long"
  arima_mod <- list(ar = .6)
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 2
  
  nlme_fit_mod <- list(fixed = sim_data ~ 1 + time + diff + act, 
                       random = ~ 1 + time | clustID)
  
  power_out <- sim_pow(fixed, random, random3 = NULL, fixed_param, 
          random_param, random_param3 = NULL,
          cov_param, k = NULL, n, p, error_var, with_err_gen, 
          data_str = data_str, pow_param = pow_param, alpha = alpha,
          pow_dist = pow_dist, pow_tail = pow_tail, 
          replicates = replicates, raw_power = FALSE, 
          nlme_fit_mod = nlme_fit_mod)
  
  expect_equal(nrow(power_out), 3)
  
  arima_fit_mod <- nlme::corAR1()
  power_out <- sim_pow(fixed, random, random3 = NULL, fixed_param, 
                       random_param, random_param3 = NULL,
                       cov_param, k = NULL, n, p, error_var, with_err_gen, 
                       data_str = data_str, pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, raw_power = FALSE, 
                       nlme_fit_mod = nlme_fit_mod, arima_fit_mod = arima_fit_mod)
  
  expect_equal(nrow(power_out), 3)
  
})

