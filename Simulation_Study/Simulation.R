# Random Effect Model Experiments ----


# Wrapper Functions
# ------------------------------------

# MC diagnostics
#--------------------------------
h <- function(chain) 1+2*sum((acf(chain, plot = F)$acf[1:30])^2)

# For MH
# --------------------------------
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

# For PMMC / CPMMC
# --------------------------------
simulation_study <- function(T_, N_, theta_0, rho, nsum, burnin, u_0, data){

  # Instantiate model ----
  rem <- normal_random_effect_model(data,
                                    theta_0 = theta_0,
                                    u_0 = u_0,
                                    rho = rho,
                                    log_theta_prior_density = function(x) dnorm(x, log = T, sd = 3),
                                    log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = T, sd=0.02),
                                    theta_proposal_sampler = function(x) rnorm(1, mean = x, sd = 0.02)
  )


  # Create results store
  Z_results <- list()
  W_results <- list()

  # Run model
  start_time <- Sys.time()
  for(i in seq_len(nsim)){
    if (i %% 100 == 0){
      current_time <- Sys.time()
      cat(paste0(">> Iteration: ",i, "\n"))
      cat(paste0(">> Time elapsed: ", current_time-start_time, " \n \n" ))

    }

    rem <- single_transition(rem)
    W_results[[i]] <- rem$log_new_marginal_estimator -
      sum(dnorm(rem$data, mean = rem$proposed_chain[[i]], sd = sqrt(2), log = T))

    Z_results[[i]] <- rem$log_old_marginal_estimator -
      sum(dnorm(rem$data, mean = rem$chain[[i]], sd = sqrt(2), log = T))
  }

  return(
    list(
      w = sapply(W_results, function(x) x[[1]]),
      z = sapply(Z_results, function(x) x[[1]]),
      object = rem
    )
  )
}


# Experiment 1
# --------------------------------
## Parameters ----

T_ <- 8192
N_ <- 80
rho <- 0.9963
nsim <- 10^4
burnin <- 100
theta_0 <- 0
# Generate data and initialise ----
u_0 <- array(rnorm(T_*N_), dim = c(N_,1,T_))
data <- rnorm(T_,0.5)

long_run <- simulation_study(T_, N_, theta_0, rho, nsum, burnin, u_0, data)


# Experiment 2
# --------------------------------
## Parameters ----

T_ <- 1024
N_ <- 19
rho <- 0.9894
nsim <- 10^4
burnin <- 100
theta_0 <- 0

# Generate data and initialise ----
u_0 <- array(rnorm(T_*N_), dim = c(N_,1,T_))
data <- rnorm(T_,0.5)

# Run comparisons
exp_2_cpm <- simulation_study(T_, N_, theta_0, rho, nsim, burnin, u_0, data)
exp_2_pm <- simulation_study(T_, N_, theta_0, 0, nsim, burnin, u_0, data)

mh <- metropolis_hastings(theta_0, log_target_density_theta, log_proposal_density, proposal_sampler)
mh <- run_chain(mh, nsim)
exp_2_mh <- mh

chain <- sapply(mh$chain, function(x) x[[1]])

h(chain)
acf(chain, plot = F)$acf










