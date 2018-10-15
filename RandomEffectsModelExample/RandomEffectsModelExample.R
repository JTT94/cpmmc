
#### Random Effects Model ####



## simulating data for T = 8192, true theta = 0.5 ##
rem_x <- rnorm(8192, 0.5, 1)
rem_y <- rnorm(8192, rem_x, 1)


## creating cpmmc object for N=80
rem80 <- cpmmc(data = rem_y,
                   theta_0 = 0,
                   u_0 = rep(0, 80),
                   rho = 0.9963,
                   marginal_estimator_func = function(data, new_theta, new_u) {
                     N <- length(new_u)
                     single_observation_likelihood <- sapply(y, function(x) {sum(dnorm(x, new_theta + new_u, 1))/N})
                     prod(single_observation_likelihood)
                   },
                   theta_prior_density = function(x) dnorm(x, 0, 3),
                   theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta),
                   theta_proposal_sampler = function(old_theta, new_theta) old_theta+rnorm(1)
)



run_mh(rem80, 100)








