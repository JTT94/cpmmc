
T_ <- 81
N_ <- 80
rho <- 0.9963
u_0 <- array(rnorm(T_*N_), dim = c(N_,1,T_))
data <- rnorm(T_,0.5)
rem <- normal_random_effect_model(data,
                                  theta_0 = 0,
                                  u_0 = u_0,
                                  rho = rho)
rem <- run_chain(rem, chain_length = 1000)

accept_chain <- sapply(rem$chain, function(x) x[[1]])
mean(data)
mean(accept_chain)

log_theta_prior_density = function(x) dnorm(x, log = T)
log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = T)
theta_proposal_sampler = function(theta) rnorm(1, mean = theta)


rem <- run_chain(rem, chain_length = 100)
get_chain_length((rem))
get_state(rem)
single_transition(rem)

profvis::profvis(
  rem <- run_chain(rem, chain_length = 10)
  )

begin_time <- Sys.time()
rem <- run_chain(rem, chain_length = 5)
end_time <- Sys.time()
print(end_time-begin_time)

rem <- run_chain(rem, chain_length = 1000)

Rprof(NULL)


accept_chain <- sapply(rem$chain, function(x) x[[1]])
mean(data)
mean(accept_chain)
plot(data)
plot(accept_chain, ty)
var(data)

# Metropolis Hastings
initial_params <- 0
log_target_density <- function(data, theta){
  (sum(dnorm(data, mean = theta, sd = sqrt(2), log = T)))
}

log_target_density_theta <- function(theta){
  log_target_density(data, theta)
}

proposal_sampler <- function(state){
  rnorm(1, mean = state)
}

log_proposal_density <- function(state, proposal){
  dnorm(proposal-state, log=T)
}

mh <- metropolis_hastings(initial_params, log_target_density_theta, log_proposal_density, proposal_sampler)

single_transition(mh)
mh <- run_chain(mh, 1000)

mean(data)
mean(sapply(get_chain(mh), function(x) x[[1]]))



mh
#
# data <- rnorm(1000, 0.5)
# cpmmc_obj <- cpmmc(data = data,
#                    theta_0 = rnorm(1,0,3),
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

