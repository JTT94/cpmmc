
#### Random Effects Model ####



## simulating data for T = 8192, true theta = 0.5 ##
rem1_x <- rnorm(8192, 0.5, 1)
rem1_y <- rnorm(8192, rem_x, 1)

rem1 <- cpmmc(data = rem1_y,
                   theta_0 = 0,
                   u_0 = 0,
                   rho = 0.9963,
                   marginal_estimator_func = function(data, new_theta, new_u) rnorm(1),
                   theta_prior_density = function(x) dnorm(x, 0, 3),
                   theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta),
                   theta_proposal_sampler = function(old_theta, new_theta) old_theta+rnorm(1)
)


