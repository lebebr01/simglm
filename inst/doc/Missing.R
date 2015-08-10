## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(simReg)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----dropout-------------------------------------------------------------
# Simulate longitudinal data
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
rand_gen <- rnorm
data_str <- "long"
temp.long <- sim_reg(fixed, random, random3 = NULL, fixed.param, random.param, random.param3 = NULL,
 cov.param, k = NULL, n, p, error_var, randCor, randCor3 = NULL, rand_dist, rand_gen,
 data_str = data_str)

# simulate missing data
temp.long.miss <- missing_data(temp.long, miss_prop = .25, type = 'dropout', clust_var = 'clustID')
head(temp.long.miss)

## ----propmissing---------------------------------------------------------
prop.table(table(temp.long.miss$missing))
prop.table(table(is.na(temp.long.miss$sim.data2)))

## ----marexamp------------------------------------------------------------
# simulate data
fixed <- ~1 + age + income
fixed.param <- c(2, 0.3, 1.3)
cov.param <- list(mean = c(0, 0), sd = c(4, 3), 
                  var.type = c("single", "single"))
n <- 150
error_var <- 3
rand_gen <- rnorm
temp.single <- sim_reg(fixed = fixed, fixed.param = fixed.param,
                       cov.param = cov.param,
                       n = n, error_var = error_var, rand_gen = rand_gen,
                       data_str = "single")

# generate missing data
miss_prop <- c(0.5, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
miss_prop <- rep(miss_prop, each = 15)
tmp.single.miss <- missing_data(temp.single, miss_prop = miss_prop, 
                                type = 'mar', miss_cov = 'income')
head(tmp.single.miss)

## ----marmisscheck--------------------------------------------------------
table(tmp.single.miss$miss_prop,is.na(tmp.single.miss$sim.data2))

## ----mcarmisssingle------------------------------------------------------
tmp.single.miss <- missing_data(temp.single, miss_prop = .25, 
                                type = 'random', clust_var = NULL)
head(tmp.single.miss)

## ----mcarmissverify------------------------------------------------------
prop.table(table(is.na(tmp.single.miss$sim.data2)))

## ----clustmcar, eval = FALSE, echo = FALSE-------------------------------
#  tmp.long.miss <- missing_data(temp.long, miss_prop = .25, type = 'random', clust_var = 'clustID')
#  head(tmp.long.miss)

