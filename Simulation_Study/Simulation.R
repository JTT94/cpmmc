# Random Effect Model Experiments ----

## Parameters ----
T_ <- 8192
N_ <- 80
rho <- 0.9963
nsim <- 10^4
burnin <- 100

# Generate data and initialise ----
u_0 <- array(rnorm(T_*N_), dim = c(N_,1,T_))
data <- rnorm(T_,0.5)

# Instantiate model ----
rem <- normal_random_effect_model(data,
                                  theta_0 = 0,
                                  u_0 = u_0,
                                  rho = rho,
                                  log_theta_prior_density = function(x) dnorm(x, log = T, sd = 3),
                                  log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = T, sd=0.02),
                                  theta_proposal_sampler = function(theta) rnorm(1, mean = theta, sd = 0.02)
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

    z <- abs(sapply((Z_results), function(x) x[[1]]))
    w <- abs(sapply((W_results), function(x) x[[1]]))

    plot(w, type= 'l', col='blue')
    points(z, type = 'l', col = 'red')


    if (i > 2*burnin){
      proposal <- sapply(rem$proposed_chain, function(x) x[[1]])
      accepted <- sapply(rem$chain, function(x) x[[1]])
      plot(proposal[burnin:nsim], type = 'l', col= 'red')
      points(accepted[burnin:nsim], type='l', col='blue')
    }


  }

  rem <- single_transition(rem)
  W_results[[i]] <- rem$log_new_marginal_estimator -
    sum(dnorm(rem$data, mean = rem$proposed_chain[[i]], sd = sqrt(2), log = T))

  Z_results[[i]] <- rem$log_old_marginal_estimator -
    sum(dnorm(rem$data, mean = rem$chain[[i]], sd = sqrt(2), log = T))
}

hist(w-z, probability = T)


