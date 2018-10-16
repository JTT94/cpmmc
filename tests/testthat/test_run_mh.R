context("run_mh")
library(cpmmc)


test_that("run_mh returns desired theta expectations (tolerance 0.02, 1000 normal mean=0.5, sd=1)", {
  data = rnorm(1000, 0.5, 1)
  cpmmc_obj <- cpmmc(data,
                     theta_0 = 0,
                     u_0 = array(rnorm(80*1*1000), dim = c(80,1,1000)),
                     rho = 0.9,
                     log_marginal_estimator_func = function(x, new_theta, new_u) sum(dnorm(x, new_theta, 1, log=TRUE)),
                     log_theta_prior_density = function(x) dnorm(x, log = TRUE),
                     log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = TRUE),
                     theta_proposal_sampler = function(theta) rnorm(1)+theta
  )

  cpmmc_obj <- run_chain(cpmmc_obj, 500)
  theta_chain <- sapply(cpmmc_obj$accept_chain, function(x) x$theta)
  expect_equal(mean(theta_chain), mean(data), tolerance=0.02)
})


test_that("run_mh returns desired theta expectations (tolerance 0.02, 1000 normal mean=1, sd=2)", {
  data = rnorm(1000, 1, 2)
  cpmmc_obj <- cpmmc(data,
                     theta_0 = 0,
                     u_0 = array(rnorm(80*1*1000), dim = c(80,1,1000)),
                     rho = 0.9,
                     log_marginal_estimator_func = function(x, new_theta, new_u) sum(dnorm(x, new_theta, 2, log=TRUE)),
                     log_theta_prior_density = function(x) dnorm(x, log = TRUE),
                     log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = TRUE),
                     theta_proposal_sampler = function(theta) rnorm(1)+theta
  )

  cpmmc_obj <- run_chain(cpmmc_obj, 500)
  theta_chain <- sapply(cpmmc_obj$accept_chain, function(x) x$theta)
  expect_equal(mean(theta_chain), mean(data), tolerance=0.02)
})


test_that("run_mh returns desired theta expectations (tolerance 0.02, 1000 normal mean=3, sd=2)", {
  data = rnorm(1000, 3, 2)
  cpmmc_obj <- cpmmc(data,
                     theta_0 = 0,
                     u_0 = array(rnorm(80*1*1000), dim = c(80,1,1000)),
                     rho = 0.9,
                     log_marginal_estimator_func = function(x, new_theta, new_u) sum(dnorm(x, new_theta, 2, log=TRUE)),
                     log_theta_prior_density = function(x) dnorm(x, log = TRUE),
                     log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = TRUE),
                     theta_proposal_sampler = function(theta) rnorm(1)+theta
  )

  cpmmc_obj <- run_chain(cpmmc_obj, 500)
  theta_chain <- sapply(cpmmc_obj$accept_chain, function(x) x$theta)
  expect_equal(mean(theta_chain), mean(data), tolerance=0.02)

})


test_that("run_mh returns desired theta expectations (tolerance 0.02, 1000 normal mean=5, sd=1)", {
  data = rnorm(1000, 5, 1)
  cpmmc_obj <- cpmmc(data,
                     theta_0 = 0,
                     u_0 = array(rnorm(80*1*1000), dim = c(80,1,1000)),
                     rho = 0.9,
                     log_marginal_estimator_func = function(x, new_theta, new_u) sum(dnorm(x, new_theta, 1, log=TRUE)),
                     log_theta_prior_density = function(x) dnorm(x, log = TRUE),
                     log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = TRUE),
                     theta_proposal_sampler = function(theta) rnorm(1)+theta
  )

  cpmmc_obj <- run_chain(cpmmc_obj, 500)
  theta_chain <- sapply(cpmmc_obj$accept_chain, function(x) x$theta)
  expect_equal(mean(theta_chain), mean(data), tolerance=0.02)
})





