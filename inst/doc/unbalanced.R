## ----seed, echo = FALSE--------------------------------------------------
set.seed(100)

## ----twolevelunbal-------------------------------------------------------------------------------
library(simglm)
fixed <- ~1 + diff + act + diff:act
random <- ~1 +  diff
fixed_param <- c(4, 6, 2.3, 7)
random_param <- list(random_var = c(7, 2), rand_gen = 'rnorm')
cov_param <- list(mean = c(0, 0), sd = c(1.5, 4), var_type = c("lvl1", "lvl2"))
n <- 150
unbal <- TRUE
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "cross"
unbalCont <- c(3, 10)
temp_cross <- sim_reg(fixed = fixed, random = random, 
                      fixed_param = fixed_param, 
                      random_param = random_param, cov_param = cov_param,
                      k = NULL, n = n, p = NULL, error_var = error_var,
                      with_err_gen = with_err_gen, data_str = data_str, 
                      unbal = TRUE, unbalCont = unbalCont)

## ----clustValue----------------------------------------------------------------------------------
table(temp_cross$clustID)

## ----bal3lvl2------------------------------------------------------------------------------------
library(simglm)
fixed <- ~1 + time + diff + act + actClust + time:act
random <- ~1 + time + diff
random3 <- ~ 1 + time
fixed_param <- c(4, 2, 6, 2.3, 7, 0)
random_param <- list(random_var = c(7, 4, 2), rand_gen = "rnorm")
random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
var_type = c("lvl1", "lvl2", "lvl3"))
k <- 10
n <- 150
unbal <- TRUE
unbal3 <- FALSE
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "long"
unbalCont <- c(min = 3, max = 10)
unbalCont3 = NULL
temp_three <- sim_reg(fixed = fixed, random = random, random3 = random3,
      fixed_param = fixed_param, random_param = random_param, 
      random_param3 = random_param3, cov_param = cov_param, k = k,
      n = n, p = p, unbal = unbal, unbal3 = unbal3, error_var = error_var,
      with_err_gen = with_err_gen, 
      data_str = data_str, unbalCont = unbalCont, unbalCont3 = unbalCont3)

## ----threecheck----------------------------------------------------------------------------------
table(temp_three$clustID)
func_temp <- function(x) length(unique(x))
tapply(temp_three$clustID, temp_three$clust3ID, func_temp)

## ----unbal3lvl2----------------------------------------------------------------------------------
library(simglm)
fixed <- ~1 + time + diff + act + actClust + time:act
random <- ~1 + time + diff
random3 <- ~ 1 + time
fixed_param <- c(4, 2, 6, 2.3, 7, 0)
random_param <- list(random_var = c(7, 4, 2), rand_gen = 'rnorm')
random_param3 <- list(random_var = c(4, 2), rand_gen = 'rnorm')
cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
var_type = c("lvl1", "lvl2", "lvl3"))
k <- 10
unbal <- TRUE
unbal3 <- TRUE
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "long"
unbalCont <- c(min = 3, max = 30)
unbalCont3 = c(min = 3, max = 10)
temp_three <- sim_reg(fixed = fixed, random = random, random3 = random3,
      fixed_param = fixed_param, random_param = random_param, 
      random_param3 = random_param3, cov_param = cov_param, k = k,
      n = NULL, p = NULL, unbal = unbal, unbal3 = unbal3, error_var = error_var,
      with_err_gen = with_err_gen, 
      data_str = data_str, unbalCont = unbalCont, unbalCont3 = unbalCont3)

## ----unbal3--------------------------------------------------------------------------------------
table(temp_three$clustID)
tapply(temp_three$clustID, temp_three$clust3ID, func_temp)

