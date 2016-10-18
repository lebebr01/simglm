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

test_that('sim_factor errors', {
  expect_error(sim_factor(n = 100, p = 3, numlevels = 4, replace = FALSE,
                          prob = c(.25, .25, .25, .25), 
                          var_type = 'lvl2'))
  expect_error(sim_factor(n = 100, numlevels = 4, replace = FALSE,
                          prob = c(.25, .25, .25, .25),
                          var_type = 'single'))
  expect_error(sim_factor(n = 100, p = 3, numlevels = 4, replace = FALSE,
                          prob = c(.25, .25, .25, .25),
                          var_type = 'lvl1'))
  expect_error(sim_factor(n = 100, p = 3, k = 30, numlevels = 4, replace = FALSE,
                          prob = c(.25, .25, .25, .25), 
                          var_type = 'lvl3'))
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

test_that('correlated fixef variables single level', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 10000
  cov_param <- list(mean = c(0, 0), sd = c(4, 3), 
                    var_type = c("single", "single"))
  cor_vars <- .6
  
  c_mat <- matrix(nrow = 2, ncol = 2)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars)[, 2:3]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .03)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), cov_param$sd, 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), cov_param$mean,
               check.attributes = FALSE, tolerance = .1)
  
  fixed <- ~ 1 + act + gpa + v4
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 10000
  cov_param <- list(mean = c(0, 0, 0), sd = c(4, 3, 2),
                    var_type = c("single", "single", 'single'))
  cor_vars <- c(.6, .4, .1)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_single(fixed, fixed_vars, n, cov_param, cor_vars)[, 2:4]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .03)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), cov_param$sd, 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), cov_param$mean,
               check.attributes = FALSE, tolerance = .1)
})

test_that('fixef correlated 2 level', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 5000
  p <- rep(10, 5000)
  data_str <- 'cross'
  cov_param <- list(mean = c(0, 0), sd = c(4, 3), 
                    var_type = c("lvl1", "lvl2"))
  cor_vars <- .6
  
  c_mat <- matrix(nrow = 2, ncol = 2)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, p, data_str, 
                            cor_vars)[, 2:3]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .03)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), cov_param$sd, 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), cov_param$mean,
               check.attributes = FALSE, tolerance = .1)
  
  fixed <- ~ 1 + act + gpa + v4
  fixed_vars <- attr(terms(fixed),"term.labels")
  n <- 5000
  p <- rep(10, 5000)
  data_str <- 'cross'
  cov_param <- list(mean = c(0, 0, 0), sd = c(4, 3, 1), 
                    var_type = c("lvl1", "lvl2", 'lvl2'))
  cor_vars <- c(.6, .7, .2)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested(fixed, fixed_vars, cov_param, n, p, data_str, 
                            cor_vars)[, 2:4]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .05)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), cov_param$sd, 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), cov_param$mean,
               check.attributes = FALSE, tolerance = .1)
})

test_that('fixef correlation three level', {
  fixed <- ~ 1 + act + gpa
  fixed_vars <- attr(terms(fixed),"term.labels")
  k <- 250
  n <- 3000
  p <- rep(10, 3000)
  data_str <- 'cross'
  cov_param <- list(mean = c(0, 0), sd = c(4, 3), 
                    var_type = c("lvl1", "lvl2"))
  cor_vars <- .3
  
  c_mat <- matrix(nrow = 2, ncol = 2)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n, p, data_str, 
                            cor_vars)[, 2:3]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .05)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), cov_param$sd, 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), cov_param$mean,
               check.attributes = FALSE, tolerance = .1)
  
  fixed <- ~ 1 + act + gpa + v4
  fixed_vars <- attr(terms(fixed),"term.labels")
  k <- 200
  n <- 2000
  p <- rep(10, 2000)
  data_str <- 'cross'
  cov_param <- list(mean = c(0, 0, 0), sd = c(4, 3, 2), 
                    var_type = c("lvl1", "lvl2", 'lvl3'))
  cor_vars <- c(.6, .7, .2)
  
  c_mat <- matrix(nrow = 3, ncol = 3)
  diag(c_mat) <- 1
  c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
  
  fixef <- sim_fixef_nested3(fixed, fixed_vars, cov_param, k, n, p, data_str, 
                            cor_vars)[, 2:4]
  
  expect_equal(cor(fixef), c_mat, 
               check.attributes = FALSE, tolerance = .05)
  expect_equal(sapply(1:ncol(fixef), function(xx) sd(fixef[, xx])), cov_param$sd, 
               check.attributes = FALSE, tolerance = .02)
  expect_equal(sapply(1:ncol(fixef), function(xx) mean(fixef[, xx])), cov_param$mean,
               check.attributes = FALSE, tolerance = .1)
})
