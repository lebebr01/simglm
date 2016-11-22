context("vary terms list")

test_that('list terms vary', {
  fixed <- ~ 1 + act + diff + numCourse + act:numCourse
  fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
  cov_param <- list(mean = c(0, 0, 0), sd = c(2, 2, 1), var_type = c("single", "single", "single"))
  n <- NULL
  error_var <- NULL
  with_err_gen <- 'rnorm'
  pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
  alpha <- .01
  pow_dist <- "t"
  pow_tail <- 2
  replicates <- 10
  terms_vary <- list(n = c(20, 40), error_var = c(5, 10),
                     fixed_param = list(c(0.5, 1.1, 0.6, 0.9, 1.1), 
                                        c(0.6, 1.1, 0.6, 0.9, 1.1)),
                     cov_param = list(list(mean = c(0, 0, 0), sd = c(2, 2, 1), 
                                           var_type = c("single", "single", "single")),
                                      list(mean = c(0.5, 0, 0), sd = c(2, 2, 1), 
                                           var_type = c("single", "single", "single"))
                     )
  )
  power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, 
                       cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, terms_vary = terms_vary)
  
  expect_equal(nrow(power_out), 4*2*2*2*2)
  
  fixed <- ~ 1 + act + diff
  fixed_param <- c(0.1, 0.5, 0.3)
  cov_param <- list(mean = c(0, 0), sd = c(2, 3), 
                    var_type = c("single", "single", "single"))
  n <- NULL
  pow_param <- c('(Intercept)', 'act', 'diff')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 10
  terms_vary <- list(n = c(20, 40),
                     fixed_param = list(c(0.5, 0.1, 0.2), 
                                        c(0.6, 0.1, 0.2)))
  
  power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
                           cov_param = cov_param, 
                           n = n, data_str = "single", 
                           pow_param = pow_param, alpha = alpha,
                           pow_dist = pow_dist, pow_tail = pow_tail, 
                           replicates = replicates, terms_vary = terms_vary)
  expect_equal(nrow(power_out), 3*2*2)
})

test_that('two level power continuous', {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time
  fixed_param <- c(0, 0.2, 0.1, 0.3, 0.05)
  random_param <- list(random_var = c(7, 4), rand_gen = "rnorm")
  cov_param <- list(mean = c(0, 0), sd = c(1, 1), var_type = c("lvl1", "lvl2"))
  n <- 75
  p <- 10
  error_var <- 1
  data_str <- "long"
  pow_param <- c('time', 'diff', 'act')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 5
  terms_vary <- list(n = c(20, 40), p = c(10, 30),
                     fixed_param = list(c(0.5, 0.1, 0.2, 0.2, .05), 
                                        c(0.6, 0.1, 0.2, 0.2, 0.05)))
  power_out <- sim_pow(fixed = fixed, random = random, 
                       fixed_param = fixed_param, 
                       random_param = random_param, cov_param = cov_param, 
                       k = NULL, n = n, p = p,
                       error_var = error_var, with_err_gen = "rnorm",
                       data_str = data_str, unbal = FALSE, 
                       pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, 
                       replicates = replicates, terms_vary = terms_vary)
  
  expect_equal(nrow(power_out), 3*2*2*2)
})

# test_that("three level power continuous", {
#   fixed <- ~1 + time + diff + act + actClust + time:act
#   random <- ~1 + time 
#   random3 <- ~ 1 + time
#   fixed_param <- c(0.3, 0.6, 0.2, 0.1, 0.25, 0.8)
#   random_param <- list(random_var = c(7, 4), rand_gen = 'rnorm')
#   random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
#   cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2), 
#                     var_type = c("lvl1", "lvl2", "lvl3"))
#   k <- 10
#   n <- 75
#   p <- 10
#   error_var <- 4
#   with_err_gen <- 'rnorm'
#   data_str <- "long"
#   pow_param <- c('time', 'diff', 'act', 'actClust')
#   alpha <- .01
#   pow_dist <- "z"
#   pow_tail <- 2
#   replicates <- 5
#   terms_vary <- list(p = c(10, 20), 
#                      random_param = list(list(random_var = c(7, 4), rand_gen = 'rnorm'),
#                                          list(random_var = c(5, 4), rand_gen = 'rnorm')))
#   power_out <- sim_pow(fixed = fixed, random = random, random3 = random3,
#                        fixed_param = fixed_param, 
#                        random_param = random_param, 
#                        random_param3 = random_param3, 
#                        cov_param = cov_param, 
#                        k = k, n = n, p = p,
#                        error_var = error_var, with_err_gen = "rnorm",
#                        data_str = data_str, unbal = FALSE, unbal3 = FALSE, 
#                        pow_param = pow_param, alpha = alpha,
#                        pow_dist = pow_dist, pow_tail = pow_tail, 
#                        replicates = replicates, 
#                        terms_vary = terms_vary)
#   
#   expect_equal(nrow(power_out), 4*2*2)
# })

test_that('two level power dich', {
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time
  fixed_param <- c(0, 0.2, 0.1, 0.3, 0.05)
  random_param <- list(random_var = c(7, 4), rand_gen = "rnorm")
  cov_param <- list(mean = c(0, 0), sd = c(3, 5), var_type = c("lvl1", "lvl2"))
  n <- 75
  p <- 10
  data_str <- "long"
  pow_param <- c('time', 'diff', 'act')
  alpha <- .01
  pow_dist <- "z"
  pow_tail <- 2
  replicates <- 2
  terms_vary <- list(fixed_param = list(c(0, 0.2, 0.1, 0.3, 0.05), 
                                        c(0.3, 0.2, 0.1, 0.3, 0.05)))
  power_out <- sim_pow_glm(fixed = fixed, random = random, 
                           fixed_param = fixed_param, 
                           random_param = random_param, cov_param = cov_param, 
                           k = NULL, n = n, p = p,
                           data_str = data_str, unbal = FALSE, 
                           pow_param = pow_param, alpha = alpha,
                           pow_dist = pow_dist, pow_tail = pow_tail, 
                           replicates = replicates, 
                           terms_vary = terms_vary)
  
  expect_equal(nrow(power_out), 3*2)
})

# test_that("three level power dich", {
#   fixed <- ~1  + diff + act + actClust
#   random <- ~1 
#   random3 <- ~ 1
#   fixed_param <- c(0.2, 0.1, 0.3, 0.05)
#   random_param <- list(random_var = 7, rand_gen = 'rnorm')
#   random_param3 <- list(random_var = 4, rand_gen = 'rnorm')
#   cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2), 
#                     var_type = c("lvl1", "lvl2", "lvl3"))
#   k <- 10
#   n <- 75
#   p <- 15
#   data_str <- "cross"
#   pow_param <- c('diff', 'act', 'actClust')
#   alpha <- .01
#   pow_dist <- "z"
#   pow_tail <- 2
#   replicates <- 2
#   terms_vary <- list(fixed_param = list(c(0, 0.2, 0.1, 0.3), 
#                                         c(0.3, 0.2, 0.1, 0.3)))
#   power_out <- sim_pow_glm(fixed = fixed, random = random, random3 = random3,
#                            fixed_param = fixed_param, 
#                            random_param = random_param, 
#                            random_param3 = random_param3, 
#                            cov_param = cov_param, 
#                            k = k, n = n, p = p,
#                            data_str = data_str, unbal = FALSE, unbal3 = FALSE, 
#                            pow_param = pow_param, alpha = alpha,
#                            pow_dist = pow_dist, pow_tail = pow_tail, 
#                            replicates = replicates, 
#                            terms_vary = terms_vary)
#   
#   expect_equal(nrow(power_out), 3*2)
# })
