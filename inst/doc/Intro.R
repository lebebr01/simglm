## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(simReg)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----install, eval=FALSE-------------------------------------------------
#  library(devtools)
#  install_github("lebebr01/simReg")
#  library(simReg)

## ----singlelevel---------------------------------------------------------
fixed <- ~ 1 + act + diff + numCourse + act:numCourse
fixed.param <- c(2, 4, 1, 3.5, 2)
cov.param <- list(mean = c(0, 0, 0), sd = c(4, 3, 3), var.type = c("single", "single", "single"))
n <- 150
error_var <- 3
rand_gen = rnorm
temp.single <- sim_reg(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param,
n = n, error_var = error_var, rand_gen = rand_gen, data_str = "single")

## ----printsinglelevel----------------------------------------------------
head(temp.single)

## ----simpregmod----------------------------------------------------------
summary(lm(sim.data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single))

## ----singlelevelfact-----------------------------------------------------
fixed <- ~ 1 + act + diff + numCourse.o + act:numCourse.o
fixed.param <- c(2, 4, 1, 3.5, 2)
cov.param <- list(mean = c(0, 0), sd = c(4, 3), var.type = c("single", "single"))
fact.vars <- list(numlevels = 5, var.type = "single", "single")
n <- 150
error_var <- 3
rand_gen = rnorm
temp.single.o <- sim_reg(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param,
n = n, error_var = error_var, rand_gen = rand_gen, data_str = "single", fact.vars = fact.vars)

## ----printsinglelevelfact------------------------------------------------
head(temp.single.o)

## ----longsim-------------------------------------------------------------
fixed <- ~1 + time + diff + act + time:act
random <- ~1 + time + diff
fixed.param <- c(4, 2, 6, 2.3, 7)
random.param <- c(7, 4, 2)
cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
n <- 150
p <- 30
error_var <- 4
randCor <- 0
rand_dist <- "norm"
rand_gen = rnorm
data_str <- "long"
temp.long <- sim_reg(fixed = fixed, random = random, 
                     fixed.param = fixed.param, 
                     random.param = random.param, cov.param = cov.param, 
                     k = NULL, n = n, p = p,
                     error_var = error_var, randCor = randCor, 
                     rand_dist = rand_dist, rand_gen = rand_gen,
                     data_str = data_str, unbal = FALSE)

## ----longdata------------------------------------------------------------
head(temp.long)

## ----lme4----------------------------------------------------------------
library(lme4)
lmer(sim.data ~ 1 + time + diff + act + time:act + (1 + time + diff | clustID),
data = temp.long)

