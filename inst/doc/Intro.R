## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(simglm)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----install, eval=FALSE-------------------------------------------------
#  library(devtools)
#  install_github("lebebr01/simglm")
#  library(simglm)

## ----singlelevel---------------------------------------------------------
fixed <- ~ 1 + act + diff + numCourse + act:numCourse
fixed_param <- c(2, 4, 1, 3.5, 2)
cov_param <- list(mean = c(0, 0, 0), sd = c(4, 3, 3), var_type = c("single", "single", "single"))
n <- 150
error_var <- 3
with_err_gen = 'rnorm'
temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                       n = n, error_var = error_var, with_err_gen = with_err_gen, 
                       data_str = "single")

## ----printsinglelevel----------------------------------------------------
head(temp_single)

## ----simpregmod----------------------------------------------------------
summary(lm(sim_data ~ 1 + act + diff + numCourse + act:numCourse, data = temp_single))

## ----singlelevelfact-----------------------------------------------------
fixed <- ~ 1 + act.o + diff.o + numCourse.o + act.o:numCourse.o
fixed_param <- c(0.8, 1, 0.2, 0.1, 0)
cov_param <- NULL
fact_vars <- list(numlevels = c(36, 8, 5), var_type = c('single', 'single', "single"))
n <- 150
error_var <- 3
with_err_gen = 'rnorm'
temp_single_o <- sim_reg(fixed = fixed, fixed_param = fixed_param, 
                         cov_param = cov_param, n = n, error_var = error_var,
                         with_err_gen = with_err_gen, data_str = "single", 
                         fact_vars = fact_vars)

## ----printsinglelevelfact------------------------------------------------
head(temp_single_o)

## ----corvars-------------------------------------------------------------
fixed <- ~ 1 + act + diff + numCourse.o + act:numCourse.o
fixed_param <- c(2, 4, 1, 3.5, 2)
cov_param <- list(mean = c(0, 0), sd = c(4, 3), var_type = c("single", "single"))
fact_vars <- list(numlevels = 5, var_type = "single")
n <- 150
error_var <- 3
with_err_gen = 'rnorm'
cor_vars <- 0.6
temp_single_o <- sim_reg(fixed = fixed, fixed_param = fixed_param, 
                         cov_param = cov_param, n = n, error_var = error_var,
                         with_err_gen = with_err_gen, data_str = "single", 
                         cor_vars = cor_vars, fact_vars = fact_vars)
cor(temp_single_o[, 2:3])

## ----longsim-------------------------------------------------------------
fixed <- ~1 + time + diff + act + time:act
random <- ~1 + time + diff
fixed_param <- c(4, 2, 6, 2.3, 7)
random_param <- list(random_var = c(7, 4, 2), rand_gen = "rnorm")
cov_param <- list(mean = c(0, 0), sd = c(1.5, 4), var_type = c("lvl1", "lvl2"))
n <- 150
p <- 30
error_var <- 4
data_str <- "long"
temp_long <- sim_reg(fixed = fixed, random = random, 
                     fixed_param = fixed_param, 
                     random_param = random_param, cov_param = cov_param, 
                     k = NULL, n = n, p = p,
                     error_var = error_var, with_err_gen = "rnorm",
                     data_str = data_str, unbal = FALSE)

## ----longdata------------------------------------------------------------
head(temp_long)

## ----lme4----------------------------------------------------------------
library(lme4)
lmer(sim_data ~ 1 + time + diff + act + time:act + (1 + time + diff | clustID),
     data = temp_long)

