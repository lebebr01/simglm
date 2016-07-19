## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(simglm)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----singlelevel---------------------------------------------------------
fixed <- ~ 1 + act + diff + numCourse + act:numCourse
fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
cov_param <- list(mean = c(0, 0, 0), sd = c(2, 2, 1), var_type = c("single", "single", "single"))
n <- 150
error_var <- 20
with_err_gen <- 'rnorm'
pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
alpha <- .01
pow_dist <- "t"
pow_tail <- 2
replicates <- 100
power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                     n = n, error_var = error_var, with_err_gen = with_err_gen, 
                     data_str = "single", pow_param = pow_param, alpha = alpha,
                     pow_dist = pow_dist, pow_tail = pow_tail, 
                     replicates = replicates)

## ----printsinglelevel----------------------------------------------------
power_out

## ----standardized--------------------------------------------------------
fixed <- ~ 1 + act + diff + numCourse + act:numCourse
fixed_param <- c(0.2, 0.4, 0.25, 0.7, 0.1)
cov_param <- list(mean = c(0, 0, 0), sd = c(1, 1, 1), var_type = c("single", "single", "single"))
n <- 150
error_var <- 1
with_err_gen <- 'rnorm'
pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
alpha <- .01
pow_dist <- "t"
pow_tail <- 2
replicates <- 100
power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single", pow_param = pow_param, alpha = alpha,
                       pow_dist = pow_dist, pow_tail = pow_tail, replicates = replicates)
power_out

## ----singlelevel_vary----------------------------------------------------
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
replicates <- 50
terms_vary <- list(n = c(20, 40, 60, 80, 100), error_var = c(5, 10, 20))
power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                     n = n, error_var = error_var, with_err_gen = with_err_gen, 
                     data_str = "single", pow_param = pow_param, alpha = alpha,
                     pow_dist = pow_dist, pow_tail = pow_tail, 
                     replicates = replicates, terms_vary = terms_vary)

## ----longsim-------------------------------------------------------------
fixed <- ~1 + time + diff + act + time:act
random <- ~1 + time
fixed_param <- c(0, 0.2, 0.1, 0.3, 0.05)
random_param <- list(random_var = c(7, 4), rand_gen = "rnorm")
cov_param <- list(mean = c(0, 0), sd = c(1, 1), var_type = c("lvl1", "lvl2"))
n <- 150
p <- 30
error_var <- 1
data_str <- "long"
pow_param <- c('time', 'diff', 'act')
alpha <- .01
pow_dist <- "z"
pow_tail <- 2
replicates <- 20
power_out <- sim_pow(fixed = fixed, random = random, 
                     fixed_param = fixed_param, 
                     random_param = random_param, cov_param = cov_param, 
                     k = NULL, n = n, p = p,
                     error_var = error_var, with_err_gen = "rnorm",
                     data_str = data_str, unbal = FALSE, pow_param = pow_param, alpha = alpha,
                     pow_dist = pow_dist, pow_tail = pow_tail, replicates = replicates)

## ----longdata------------------------------------------------------------
power_out

## ----three---------------------------------------------------------------
fixed <- ~1 + time + diff + act + actClust + time:act
random <- ~1 + time 
random3 <- ~ 1 + time
fixed_param <- c(4, 2, 6, 2.3, 7, 0)
random_param <- list(random_var = c(7, 4), rand_gen = 'rnorm')
random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2), 
var_type = c("lvl1", "lvl2", "lvl3"))
k <- 10
n <- 150
p <- 30
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "long"
pow_param <- c('time', 'diff', 'act', 'actClust')
alpha <- .01
pow_dist <- "z"
pow_tail <- 2
replicates <- 5
power_out <- sim_pow(fixed = fixed, random = random, random3 = random3,
                     fixed_param = fixed_param, 
                     random_param = random_param, random_param3 = random_param3, 
                     cov_param = cov_param, 
                     k = k, n = n, p = p,
                     error_var = error_var, with_err_gen = "rnorm",
                     data_str = data_str, unbal = FALSE, unbal3 = FALSE, 
                     pow_param = pow_param, alpha = alpha,
                     pow_dist = pow_dist, pow_tail = pow_tail, replicates = replicates)
power_out

## ----singlelogistic------------------------------------------------------
fixed <- ~ 1 + act + diff
fixed_param <- c(0.1, 0.5, 0.3)
cov_param <- list(mean = c(0, 0), sd = c(2, 4), 
                  var_type = c("single", "single", "single"))
n <- 50
pow_param <- c('(Intercept)', 'act', 'diff')
alpha <- .01
pow_dist <- "z"
pow_tail <- 2
replicates <- 100

power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
                         cov_param = cov_param, 
                         n = n, data_str = "single", 
                         pow_param = pow_param, alpha = alpha,
                         pow_dist = pow_dist, pow_tail = pow_tail, 
                         replicates = replicates)
power_out

## ----singlelogistic_vary-------------------------------------------------
fixed <- ~ 1 + act + diff
fixed_param <- c(0.1, 0.5, 0.3)
cov_param <- list(mean = c(0, 0), sd = c(2, 4), 
                  var_type = c("single", "single", "single"))
n <- NULL
pow_param <- c('(Intercept)', 'act', 'diff')
alpha <- .01
pow_dist <- "z"
pow_tail <- 2
replicates <- 100
terms_vary = list(n = c(25, 50, 100))

power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
                         cov_param = cov_param, 
                         n = n, data_str = "single", 
                         pow_param = pow_param, alpha = alpha,
                         pow_dist = pow_dist, pow_tail = pow_tail, 
                         replicates = replicates, terms_vary = terms_vary)
power_out

