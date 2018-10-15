
#### Random Effects Model ####



## simulating data for T = 8192, true theta = 0.5 ##
rem_x <- rnorm(8192, 0.5, 1)
rem_y <- rnorm(8192, rem_x, 1)


## creating cpmmc object for N=80, T= 8192, p=1
rem80 <- cpmmc(data = rem_y,
                   theta_0 = 0,
                   u_0 = lapply(1:8192, matrix, data= 0, nrow=80, ncol=1),
                   rho = 0.9963,
                   marginal_estimator_func = function(data, theta, u) {
                     N <- length(u)
                     single_observation_likelihood <- sapply(data, function(x) {sum(dnorm(x, theta + u, 1))/N})
                     sum(log(single_observation_likelihood))
                   },
                   theta_prior_density = function(x) {dnorm(x, 0, 3, log = TRUE)},
                   theta_proposal_density = function(old_theta, new_theta) {dnorm(new_theta-old_theta, log = TRUE)},
                   theta_proposal_sampler = function(theta) {theta+rnorm(1)}
)



run_mh(rem80, 100)








