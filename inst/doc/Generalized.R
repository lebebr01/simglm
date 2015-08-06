## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(simReg)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----singlelogistic------------------------------------------------------
fixed <- ~ 1 + act + diff
fixed.param <- c(2, 0.5, 0.3)
cov.param <- list(mean = c(0, 0), sd = c(4, 3), 
                  var.type = c("single", "single", "single"))
n <- 150

temp.single <- sim_glm(fixed = fixed, fixed.param = fixed.param, 
                       cov.param = cov.param, 
                       n = n, data_str = "single")
head(temp.single)

## ----twologistic---------------------------------------------------------
# Longitudinal linear mixed model example
fixed <- ~1 + diff + act
random <- ~1 
fixed.param <- c(2, 0.5, 0.3)
random.param <- c(7)
cov.param <- list(mean = c(0, 0), sd = c(2, 1.4), 
                  var.type = c("lvl1", "lvl2"))
n <- 150
p <- 30
randCor <- 0
rand_dist <- "norm"
data_str <- "cross"
temp.cross <- sim_glm(fixed, random, random3 = NULL, fixed.param,
                     random.param, random.param3 = NULL,
                     cov.param, k = NULL, n, p, randCor, 
                     randCor3 = NULL, rand_dist, 
                     data_str = data_str)

## ----threelogistic-------------------------------------------------------
fixed <- ~1 + diff + act + actClust
random <- ~1
random3 <- ~ 1
fixed.param <- c(4, 0.8, 0.15, 1.1)
random.param <- c(7)
random.param3 <- c(4)
cov.param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
                  var.type = c("lvl1", "lvl2", "lvl3"))
k <- 10
n <- 150
p <- 30
randCor <- 0
randCor3 <- 0
rand_dist <- "norm"
data_str <- "cross"
temp.three <- sim_glm(fixed, random, random3, fixed.param, random.param,
                      random.param3, cov.param, k, n, p, randCor, randCor3, 
                      rand_dist, data_str = data_str)
head(temp.three)

