context('fixef_sim')

test_that('sim_factor are discrete', {
  expect_length(table(sim_factor(n = 100, numlevels = 4, 
                                 prob = c(.25, .25, .25, .25), 
                          var_type = 'single')), 4)
  expect_length(table(sim_factor(n = 100, p = rep(3, 100),numlevels = 4, 
                                 prob = c(.25, .25, .25, .25), 
                                     var_type = 'lvl1')), 4)
  expect_length(table(sim_factor(n = 100, p = rep(3, 100), numlevels = 4, 
                                 prob = c(.25, .25, .25, .25), 
                                     var_type = 'lvl2')), 4)
  expect_length(table(sim_factor(n = 100, p = rep(3, 100), k = 30, 
                                 numlevels = 4, 
                                 prob = c(.25, .25, .25, .25), 
                                     var_type = 'lvl3')), 4)
})

test_that('sim_factor errors', {
  expect_error(sim_factor(n = 100, p = rep(3, 100), numlevels = 4, 
                          replace = FALSE,
                          prob = c(.25, .25, .25, .25), 
                          var_type = 'lvl2'))
  expect_error(sim_factor(n = 100, numlevels = 4, replace = FALSE,
                          prob = c(.25, .25, .25, .25),
                          var_type = 'single'))
  expect_error(sim_factor(n = 100, p = rep(3, 100), numlevels = 4, 
                          replace = FALSE,
                          prob = c(.25, .25, .25, .25),
                          var_type = 'lvl1'))
  expect_error(sim_factor(n = 100, p = rep(3, 100), k = 30, numlevels = 4, 
                          replace = FALSE,
                          prob = c(.25, .25, .25, .25), 
                          var_type = 'lvl3'))
})

test_that('sim_continuous are continuous', {
  expect_length(table(sim_continuous(n = 100, dist_fun = 'rnorm', 
                                     mean = 0, sd = 1, 
                                     var_type = 'single')), 100)
  expect_length(table(sim_continuous(n = 100, p = rep(3, 100), 
                                     dist_fun = 'rnorm', mean = 0, sd = 1, 
                                     var_type = 'lvl1')), 300)
  expect_length(table(sim_continuous(n = 100, p = rep(3, 100), 
                                     dist_fun = 'rnorm', mean = 0, sd = 1, 
                                     var_type = 'lvl2')), 100)
  expect_length(table(sim_continuous(n = 100, p = rep(3, 100), k = 30, 
                                     dist_fun = 'rnorm',
                                     mean = 0, sd = 1, 
                                     var_type = 'lvl3')), 30)
})

test_that('correlated fixef variables single level', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 10000
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("single", "single"),
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 3)))
  cor_vars <- .6
  
  c_mat <- matrix(nrow = 2, ncol = 2)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars)[, 2:3]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .03)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), 
               c(4, 3), 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), 
               c(0, 0),
               check.attributes = FALSE, tolerance = .1)
  
  fixed <- ~ 1 + act + gpa + v4
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 10000
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("single", "single", 'single'),
                    opts = list(list(mean = 0, sd = 4), list(mean = 0, sd = 3),
                                list(mean = 0, sd = 2)))
  cor_vars <- c(.6, .4, .1)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars)[, 2:4]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .03)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), 
               c(4, 3, 2), 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), 
               c(0, 0, 0),
               check.attributes = FALSE, tolerance = .1)
})

test_that('fixef correlated 2 level', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 5000
  p <- rep(10, 5000)
  data_str <- 'cross'
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("lvl1", "lvl2"),
                    opts = list(list(mean = 0, sd = 4), 
                                list(mean = 0, sd = 3)))
  cor_vars <- .6
  
  c_mat <- matrix(nrow = 2, ncol = 2)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, p, data_str, 
                            cor_vars)[, 2:3]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .03)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), 
               c(4, 3), 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), 
               c(0, 0),
               check.attributes = FALSE, tolerance = .1)
  
  fixed <- ~ 1 + act + gpa + v4
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 5000
  p <- rep(10, 5000)
  data_str <- 'cross'
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("lvl1", "lvl2", 'lvl2'),
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 3),
                                list(mean = 0, sd = 1)))
  cor_vars <- c(.6, .7, .2)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, p, data_str, 
                            cor_vars)[, 2:4]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .05)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), 
               c(4, 3, 1), 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), 
               c(0, 0, 0),
               check.attributes = FALSE, tolerance = .1)
})

test_that('fixef correlation three level', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  k <- 250
  n <- 3000
  p <- rep(10, 3000)
  data_str <- 'cross'
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("lvl1", "lvl2"),
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 3)))
  cor_vars <- .3
  
  c_mat <- matrix(nrow = 2, ncol = 2)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n, p, data_str, 
                            cor_vars)[, 2:3]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .05)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), 
               c(4, 3), 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), 
               c(0, 0),
               check.attributes = FALSE, tolerance = .1)
  
  fixed <- ~ 1 + act + gpa + v4
  fixed_vars <- attr(terms(fixed),"term.labels")
  k <- 200
  n <- 2000
  p <- rep(10, 2000)
  data_str <- 'cross'
  cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                    var_type = c("lvl1", "lvl2", 'lvl3'),
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 3),
                                list(mean = 0, sd = 2)))
  cor_vars <- c(.6, .7, .2)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n, p, data_str, 
                            cor_vars)[, 2:4]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .05)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), 
               c(4, 3, 2), 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), 
               c(0, 0, 0),
               check.attributes = FALSE, tolerance = .1)
})

test_that('.f and .c are expanded but not .o', {
  fixed <- ~ 1 + act.o + diff.o + numCourse.f + act.o:numCourse.f
  fixed_param <- c(0.8, 1, 0.2, 0.1, 0, 0.15, 0.2, 0.5, 0.02, -0.6, -0.1)
  cov_param <- NULL
  fact_vars <- list(numlevels = c(36, 8, 5), 
                    var_type = c('single', 'single', "single"))
  n <- 150
  error_var <- 3
  with_err_gen = 'rnorm'
  fixef_single <- sim_fixef_single(fixed = fixed, 
                                fixed_vars = attr(terms(fixed),"term.labels"), 
                           cov_param = cov_param, n = n, fact_vars = fact_vars)
  fact_vars <- list(numlevels = c(36, 8, 5), var_type = c('lvl2', 'lvl1', "lvl1"))
  fixef_nested <- sim_fixef_nested(fixed = fixed, 
                                fixed_vars = attr(terms(fixed),"term.labels"), 
                                   p = rep(4, n), cov_param = cov_param, n = n, 
                                fact_vars = fact_vars,
                                   data_str = 'cross')
  fixef_nested3 <- sim_fixef_nested3(fixed = fixed, 
                                  fixed_vars = attr(terms(fixed),"term.labels"), 
                                       p = rep(4, n), k = 10, cov_param = cov_param, 
                                  n = n, fact_vars = fact_vars,
                                       data_str = 'cross')
  
  fixed <- ~ 1 + time + act.o + diff.o + numCourse.f + act.o:numCourse.f
  fixed_param <- c(0.8, 1, 0.2, 0.1, 0, 0.15, 0.2, 0.5, 0.02, -0.6, -0.1, 0.2)
  fixef_nested_l <- sim_fixef_nested(fixed = fixed, 
                                  fixed_vars = attr(terms(fixed),"term.labels"), 
                                  p = rep(4, n), cov_param = cov_param, n = n, 
                                  fact_vars = fact_vars,
                                   data_str = 'long')
  
  fixef_nested3_l <- sim_fixef_nested3(fixed = fixed, 
                                fixed_vars = attr(terms(fixed),"term.labels"), 
                                p = rep(4, n), k = 10, cov_param = cov_param, 
                                n = n, fact_vars = fact_vars,
                                data_str = 'long')
  
  expect_equal(ncol(fixef_single), 11)
  expect_equal(ncol(fixef_nested), 11)
  expect_equal(ncol(fixef_nested3), 11)
  expect_equal(ncol(fixef_nested_l), 12)
  expect_equal(ncol(fixef_nested3_l), 12)
  
})
