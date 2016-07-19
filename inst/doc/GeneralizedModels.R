## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(simglm)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----singlelogistic------------------------------------------------------
fixed <- ~ 1 + act + diff
fixed_param <- c(2, 0.5, 0.3)
cov_param <- list(mean = c(0, 0), sd = c(4, 3), 
                  var_type = c("single", "single", "single"))
n <- 150

temp_single <- sim_glm(fixed = fixed, fixed_param = fixed_param, 
                       cov_param = cov_param, 
                       n = n, data_str = "single")
head(temp_single)

## ----twologistic---------------------------------------------------------
# Longitudinal linear mixed model example
fixed <- ~1 + diff + act
random <- ~1 
fixed_param <- c(2, 0.5, 0.3)
random_param <- list(random_var = 7, rand_gen = "rnorm", ther_sim = TRUE)
cov_param <- list(mean = c(0, 0), sd = c(2, 1.4), 
                  var_type = c("lvl1", "lvl2"))
n <- 150
p <- 30
data_str <- "cross"
temp_cross <- sim_glm(fixed, random, random3 = NULL, fixed_param,
                     random_param, random_param3 = NULL,
                     cov_param, k = NULL, n, p,
                     data_str = data_str)
head(temp_cross)

## ----threelogistic-------------------------------------------------------
fixed <- ~1 + diff + act + actClust
random <- ~1
random3 <- ~ 1
fixed_param <- c(4, 0.8, 0.15, 1.1)
random_param <- list(random_var = 7, rand_gen = "rnorm")
random_param3 <- list(random_var = 4, rand_gen = "rnorm")
cov_param <- list(mean = c(0, 0, 0), sd = c(1.5, 4, 2),
                  var_type = c("lvl1", "lvl2", "lvl3"))
k <- 10
n <- 150
p <- 30
data_str <- "cross"
temp_three <- sim_glm(fixed, random, random3, fixed_param, random_param,
                      random_param3, cov_param, k, n, p, data_str = data_str)
head(temp_three)

