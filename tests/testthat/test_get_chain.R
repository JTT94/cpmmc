context("get_chain")
library(cpmmc)


test_that("Test for warning message", {

  cpmmc_obj <- cpmmc(data = rnorm(1000, 0.5, 1),
                     theta_0 = 0,
                     u_0 = array(rnorm(80*1*1000), dim = c(80,1,1000)),
                     rho = 0.9,
                     log_marginal_estimator_func = function(x, new_theta, new_u) sum(dnorm(x, new_theta, 1, log=TRUE)),
                     log_theta_prior_density = function(x) dnorm(x, log = TRUE),
                     log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = TRUE),
                     theta_proposal_sampler = function(theta) rnorm(1)+theta
  )

  expect_error(get_chain(cpmmc_obj, "wanted"), "The chain wanted is not available.")

})



