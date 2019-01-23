context('data_glm')

test_that('probabilities between 0 and 1', {
  Xmat <- cbind(rnorm(50), rnorm(50), rnorm(50))
  beta <- c(4, 3, 1)
  expect_gte(min(data_glm_single(Xmat, beta, 50, 
                                 outcome_type = 'logistic')$logistic), 0)
  expect_lte(max(data_glm_single(Xmat, beta, 50, 
                                 outcome_type = 'logistic')$logistic), 1)
  
  # z_mat <- Xmat[, 1]
  # rand_eff <- c(rnorm(50))
  # expect_gte(min(data_glm_nested(Xmat, z_mat, beta, rand_eff, 10, 5)$logistic), 0)
})

test_that('correct length', {
  Xmat <- cbind(rnorm(50), rnorm(50), rnorm(50))
  beta <- c(4, 3, 1)
  expect_equal(nrow(data_glm_single(Xmat, beta, 50, outcome_type = 'logistic')), 50)
})

test_that('sim_data are 0 or 1s inclusive', {
  Xmat <- cbind(rnorm(50), rnorm(50), rnorm(50))
  beta <- c(4, 3, 1)
  expect_length(table(data_glm_single(Xmat, beta, 50, outcome_type = 'logistic')$sim_data), 2)
})

context('data_reg')

test_that('correct length', {
  Xmat <- cbind(rnorm(50), rnorm(50), rnorm(50))
  beta <- c(4, 3, 1)
  err <- rnorm(50)
  expect_equal(nrow(data_reg_single(Xmat, beta, 50, err)), 50)
})

test_that('Fbeta + err = sim_data', {
  Xmat <- cbind(rnorm(50), rnorm(50), rnorm(50))
  beta <- c(4, 3, 1)
  err <- rnorm(50)
  tmp <- data_reg_single(Xmat, beta, 50, err)
  expect_equal(tmp[, 1] + tmp[, 2], tmp[, 3])
})


