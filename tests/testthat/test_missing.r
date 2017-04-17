context("missing data")

test_that("dropout missing occurring", {
  # Simulate longitudinal data
  fixed <- ~1 + time + diff + act + time:act
  random <- ~1 + time + diff
  fixed_param <- c(4, 2, 6, 2.3, 7)
  random_param <- list(random_var = c(7, 4, 2), rand_gen = "rnorm")
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
  
  # simulate missing data
  temp_long_miss <- missing_data(temp_long, miss_prop = .25, type = 'dropout', 
                                 clust_var = 'clustID', within_id = 'withinID')
  
  expect_equal(as.numeric(prop.table(table(temp_long_miss$missing))[2]),
               .25, 
               tolerance = .05)
  
  temp_long_miss <- missing_data(temp_long, miss_prop = .05, type = 'dropout', 
                                 clust_var = 'clustID', within_id = 'withinID')
  
  expect_equal(as.numeric(prop.table(table(temp_long_miss$missing))[2]),
               .05, 
               tolerance = .05)
})

test_that("MAR missing", {
  set.seed(1985)
  # simulate data
  fixed <- ~1 + age + income
  fixed_param <- c(2, 0.3, 1.3)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("single", "single"), 
                    opts = list(list(mean = 0, sd = 4),
                                list(mean = 0, sd = 2)))
  n <- 4500
  error_var <- 3
  with_err_gen <- 'rnorm'
  temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                         cov_param = cov_param,
                         n = n, error_var = error_var, with_err_gen = with_err_gen,
                         data_str = "single")
  
  # generate missing data
  miss_prop <- c(0.5, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
  miss_prop <- rep(miss_prop, each = 450)
  tmp_single_miss <- missing_data(temp_single, miss_prop = miss_prop, 
                                  type = 'mar', miss_cov = 'income')
  
  diff <- sum(abs(prop.table(table(tmp_single_miss$miss_prop, 
                   is.na(tmp_single_miss$sim_data2)), 1)[,2] 
  - unique(sort(miss_prop))))
  
  expect_equal(diff, .1, tolerance = .05)
})

test_that("mcar missing", {
  # simulate data
  fixed <- ~1 + age + income
  fixed_param <- c(2, 0.3, 1.3)
  cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                    var_type = c("single", "single"),
                    opts = list(list(mean = 0, sd = 4), 
                                list(mean = 0, sd = 3)))
  n <- 4500
  error_var <- 3
  with_err_gen <- 'rnorm'
  temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                         cov_param = cov_param,
                         n = n, error_var = error_var, with_err_gen = with_err_gen,
                         data_str = "single")
  tmp_single_miss <- missing_data(temp_single, miss_prop = .25, 
                                  type = 'random', clust_var = NULL)
  expect_equal(as.numeric(prop.table(table(is.na(tmp_single_miss$sim_data2)))[2]), 
               .25, tolerance = .05)
})

