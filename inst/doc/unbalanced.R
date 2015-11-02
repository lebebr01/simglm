## ----seed, echo = FALSE--------------------------------------------------
set.seed(100)

## ----twolevelunbal-------------------------------------------------------
library(simglm)
fixed <- ~1 + diff + act + diff:act
random <- ~1 +  diff
fixed.param <- c(4, 6, 2.3, 7)
random_param <- list(random.param = c(7, 2), rand_gen = 'rnorm')
cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
n <- 150
unbal <- TRUE
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "cross"
unbalCont <- c(3, 10)
temp.cross <- sim_reg(fixed = fixed, random = random, 
                      fixed.param = fixed.param, 
                      random_param = random_param, cov.param = cov.param,
                      k = NULL, n = n, p = NULL, error_var = error_var,
                      with_err_gen = with_err_gen, data_str = data_str, 
                      unbal = TRUE, unbalCont = unbalCont)

## ----clustValue----------------------------------------------------------
table(temp.cross$clustID)

## ----bal3lvl2------------------------------------------------------------
library(simglm)
fixed <- ~1 + time + diff + act + actClust + time:act
random <- ~1 + time + diff
random3 <- ~ 1 + time
fixed.param <- c(4, 2, 6, 2.3, 7, 0)
random_param <- list(random.param = c(7, 4, 2), rand_gen = "rnorm")
random_param3 <- list(random.param = c(4, 2), rand_gen = 'rnorm')
cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
var.type = c("lvl1", "lvl2", "lvl3"))
k <- 10
n <- 150
unbal <- TRUE
unbal3 <- FALSE
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "long"
unbalCont <- c(min = 3, max = 10)
unbalCont3 = NULL
temp.three <- sim_reg(fixed = fixed, random = random, random3 = random3,
      fixed.param = fixed.param, random_param = random_param, 
      random_param3 = random_param3, cov.param = cov.param, k = k,
      n = n, p = p, unbal = unbal, unbal3 = unbal3, error_var = error_var,
      with_err_gen = with_err_gen, 
      data_str = data_str, unbalCont = unbalCont, unbalCont3 = unbalCont3)

## ----threecheck----------------------------------------------------------
table(temp.three$clustID)
func.temp <- function(x) length(unique(x))
tapply(temp.three$clustID, temp.three$clust3ID, func.temp)

## ----unbal3lvl2----------------------------------------------------------
library(simglm)
fixed <- ~1 + time + diff + act + actClust + time:act
random <- ~1 + time + diff
random3 <- ~ 1 + time
fixed.param <- c(4, 2, 6, 2.3, 7, 0)
random_param <- list(random.param = c(7, 4, 2), rand_gen = 'rnorm')
random_param3 <- list(random.param = c(4, 2), rand_gen = 'rnorm')
cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
var.type = c("lvl1", "lvl2", "lvl3"))
k <- 10
unbal <- TRUE
unbal3 <- TRUE
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "long"
unbalCont <- c(min = 3, max = 30)
unbalCont3 = c(min = 3, max = 10)
temp.three <- sim_reg(fixed = fixed, random = random, random3 = random3,
      fixed.param = fixed.param, random_param = random_param, 
      random_param3 = random_param3, cov.param = cov.param, k = k,
      n = NULL, p = NULL, unbal = unbal, unbal3 = unbal3, error_var = error_var,
      with_err_gen = with_err_gen, 
      data_str = data_str, unbalCont = unbalCont, unbalCont3 = unbalCont3)

## ----unbal3--------------------------------------------------------------
table(temp.three$clustID)
func.temp <- function(x) length(unique(x))
tapply(temp.three$clustID, temp.three$clust3ID, func.temp)

