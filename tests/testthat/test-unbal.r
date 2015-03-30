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
  errorVar <- 4
  randCor <- 0
  rand.dist <- "norm"
  err.dist <- "norm"
  serCor <- "ID"
  serCorVal <- NULL
  data.str <- "cross"
  unbalCont <- NULL
  expect_error(sim.reg(fixed = fixed, random = random, fixed.param = fixed.param, 
                       random.param = random.param, cov.param = cov.param, k = NULL, 
                       n = n, p = NULL, errorVar = errorVar, randCor = randCor, 
                       rand.dist = rand.dist, err.dist = err.dist, serCor = serCor, 
                       serCorVal = serCorVal, data.str = data.str, unbal = TRUE, 
                       unbalCont = unbalCont),
               "Must specify unbalCont when unbal = TRUE")
})
