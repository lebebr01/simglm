context("unbal")

test_that("return error",{
  fixed <- ~1 + diff + act + diff:act
  random <- ~1 +  diff
  fixed.param <- c(4, 6, 2.3, 7)
  random.param <- c(7, 2)
  cov.param <- list(mean = c(0, 0), sd = c(1.5, 4), var.type = c("lvl1", "lvl2"))
  n <- 150
  p <- 30
  unbal <- TRUE
  error_var <- 4
  randCor <- 0
  rand.dist <- "norm"
  rand_gen <- rnorm
  data_str <- "cross"
  unbalCont <- NULL
  expect_error(sim_reg(fixed = fixed, random = random, fixed.param = fixed.param, 
                       random.param = random.param, cov.param = cov.param, k = NULL, 
                       n = n, p = NULL, error_var = error_var, randCor = randCor, 
                       rand.dist = rand.dist, rand_gen = rand_gen, data_str = data_str, unbal = TRUE, 
                       unbalCont = unbalCont),
               "Must specify unbalCont when unbal = TRUE")
})
