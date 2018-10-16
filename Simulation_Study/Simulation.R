# Random Effect Model Experiments ----


# Wrapper Functions
# ------------------------------------


# Calculate acceptance probability
acceptance_prob <- function(object) {
  theta_chain <- sapply(object$object$chain, function(x) x[[1]])
  ac_num <- 0
  sim_num <- length(theta_chain) - 1
  for (i in (seq_len(length(theta_chain)-1))) {
    if (theta_chain[i] != theta_chain[i+1]) {
      ac_num <- ac_num + 1
    }
  }
  return(ac_num/sim_num)
}


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


# Experiment 1.1
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
data <- rnorm(T_,0.5, sd = sqrt(2))

long_run <- simulation_study(T_, N_, theta_0, rho, nsum, burnin, u_0, data)
serialize_robject("./Simulation_Study/long_run_CPM", long_run_CPM)
# Time elapsed: 34.5729867140452 mins
long_run_PM <- simulation_study(T_, N_, theta_0, 0, nsum, burnin, u_0, data)
serialize_robject("./Simulation_Study/long_run_PM", long_run_PM)
# Time elapsed: 29.9212190111478 mins
mh <- metropolis_hastings(theta_0, log_target_density_theta, log_proposal_density, proposal_sampler)
long_run_MH <- run_chain(mh, nsim)
serialize_robject("./Simulation_Study/long_run_MH", long_run_MH)
# Time elapsed: 6.403306 secs



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
data <- rnorm(T_,0.5, sd = sqrt(2))

# Run comparisons
exp_2_cpm <- simulation_study(T_, N_, theta_0, rho, nsim, burnin, u_0, data)
exp_2_pm <- simulation_study(T_, N_, theta_0, 0, nsim, burnin, u_0, data)

mh <- metropolis_hastings(theta_0, log_target_density_theta, log_proposal_density, proposal_sampler)
mh <- run_chain(mh, nsim)
exp_2_mh <- mh

serialize_robject("./Simulation_Study/cpm_exp2", exp_2_cpm)
serialize_robject("./Simulation_Study/pm_exp2", exp_2_pm)
serialize_robject("./Simulation_Study/mh_exp2", exp_2_mh)


# Experiment 3
# --------------------------------
## Parameters ----

T_ <- 2048
N_ <- 28
rho <- 0.9925
nsim <- 10^4
burnin <- 100
theta_0 <- 0

# Generate data and initialise ----
u_0 <- array(rnorm(T_*N_), dim = c(N_,1,T_))
data <- rnorm(T_,0.5, sd = sqrt(2))

# Run comparisons
exp_3_cpm <- simulation_study(T_, N_, theta_0, rho, nsim, burnin, u_0, data)
exp_3_pm <- simulation_study(T_, N_, theta_0, 0, nsim, burnin, u_0, data)

mh <- metropolis_hastings(theta_0, log_target_density_theta, log_proposal_density, proposal_sampler)
mh <- run_chain(mh, nsim)
exp_3_mh <- mh

serialize_robject("./Simulation_Study/cpm_exp3", exp_3_cpm)
serialize_robject("./Simulation_Study/pm_exp3", exp_3_pm)
serialize_robject("./Simulation_Study/mh_exp3", exp_3_mh)



# Experiment 4
# --------------------------------
## Parameters ----

T_ <- 4096
N_ <- 39
rho <- 0.9947
nsim <- 10^4
burnin <- 100
theta_0 <- 0

# Generate data and initialise ----
u_0 <- array(rnorm(T_*N_), dim = c(N_,1,T_))
data <- rnorm(T_,0.5, sd = sqrt(2))

# Run comparisons
exp_4cpm <- simulation_study(T_, N_, theta_0, rho, nsim, burnin, u_0, data)
exp_4_pm <- simulation_study(T_, N_, theta_0, 0, nsim, burnin, u_0, data)

mh <- metropolis_hastings(theta_0, log_target_density_theta, log_proposal_density, proposal_sampler)
mh <- run_chain(mh, nsim)
exp_4_mh <- mh

serialize_robject("./Simulation_Study/cpm_exp4", exp_4_cpm)
serialize_robject("./Simulation_Study/pm_exp4", exp_4_pm)
serialize_robject("./Simulation_Study/mh_exp4", exp_4_mh)



