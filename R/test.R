

# importance_estimator <- function(y_t, theta, new_u){
#
#   # vectorised function for each data point
#   func <- function(y_t, ind){
#     u_t <- (as.matrix(new_u[,,ind]))
#     sum(dnorm(y_t, mean = u_t + theta)) / length(u_t)
#   }
#
#   sum(log(mapply( func,  y_t, seq_len(dim(new_u)[3]))))
# }
#
# data <- rnorm(1000, 0.5)
# cpmmc_obj <- cpmmc(data = data,
#                    theta_0 = 0,
#                    u_0 = array(rnorm(1000*80), dim = c(80,1,1000)),
#                    rho = 0.9,
#                    log_marginal_estimator_func = importance_estimator,
#                    log_theta_prior_density = function(x) dnorm(x, log = T),
#                    log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = T),
#                    theta_proposal_sampler = function(theta) rnorm(1, mean = theta)
# )
#
#
# before_time <- Sys.time()
# cpmmc_obj <- run_mh(cpmmc_obj, nsim = 10^4)
# after_time <- Sys.time()
# print((after_time - before_time))
#
#
# theta_chain <- sapply(cpmmc_obj$accept_chain, function(x) x$theta)
# prop_theta_chain <- sapply(cpmmc_obj$proposed_chain, function(x) x$theta)
# mean(theta_chain)
# length(theta_chain)
# dt <- data.table::data.table(prop_chain = prop_theta_chain, accept_chain = theta_chain)
# dt <- dt[1:10000]
# dt
# data.table::fwrite(dt, file ="./output/output_chains.csv" )
# plot(theta_chain, type='l')
# points(prop_theta_chain, type='l', col='blue')
