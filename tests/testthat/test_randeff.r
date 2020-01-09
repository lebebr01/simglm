# context('ranef_error')
# 
# test_that('correct length', {
#   expect_equal(length(sim_rand_eff(random_var = 3, n = 25, rand_gen = 'rnorm')), 
#                25)
# })
# 
# test_that('var random_var', {
#   expect_equal(as.numeric(var(sim_rand_eff(random_var = 3, n = 100000,
#                                   rand_gen = 'rnorm', ther_sim = TRUE))),
#                expected = 3,
#                tolerance = .05)
# })
# 
# test_that('skew', {
#   expect_gt(e1071::skewness(sim_rand_eff(random_var = 3, n = 500, 
#                                          rand_gen = 'rchisq', df = 1)),
#             1)
#   expect_gt(e1071::kurtosis(sim_rand_eff(random_var = 3, n = 500, 
#                                          rand_gen = 'rt', df = 3)),
#             1)
# })
# 
# test_that('correlated random effects', {
#   cor_vars <- .4
#   random_var <- c(4, 2)
#   
#   rand_eff <- sim_rand_eff(random_var = random_var, n = 5000,
#                            rand_gen = 'rnorm', cor_vars = cor_vars)
#   c_mat <- matrix(nrow = 2, ncol = 2)
#   diag(c_mat) <- 1
#   c_mat[upper.tri(c_mat)] <- c_mat[lower.tri(c_mat)] <- cor_vars
#   
#   expect_equal(cor(rand_eff), c_mat,
#                check.attributes = FALSE, tolerance = .05)
#   expect_equal(sapply(1:ncol(rand_eff), function(xx) var(rand_eff[, xx])),
#                random_var, 
#                check.attributes = FALSE, tolerance = .1)
# })
