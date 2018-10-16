
#### Random Effects Model ####



## simulating data for T = 8192, true theta = 0.5 ##
rem_x <- rnorm(8192, 0.5, 1)
rem_y <- rnorm(8192, rem_x, 1)


## function for IS estimation ##
importance_estimator <- function(y_t, theta, new_u){

  # vectorised function for each data point
  func <- function(y_t, ind){
    u_t <- (as.matrix(new_u[,,ind]))
    sum(dnorm(y_t, mean = u_t + theta)) / length(u_t)
  }

  sum(log(mapply(func,  y_t, seq_len(dim(new_u)[3]))))
}


#### creating cpmmc object for N=80, T=8192, p=1, rho=0.9963 ####
rem80 <- cpmmc(data = rem_y,
               theta_0 = rnorm(1, 0, 3),
               u_0 = array(rnorm(80*1*8192), dim = c(80,1,8192)),
               rho = 0.9963,
               log_marginal_estimator_func = importance_estimator,
               log_theta_prior_density = function(x) {dnorm(x, 0, 3, log = TRUE)},
               log_theta_proposal_density = function(old_theta, new_theta) {dnorm(new_theta-old_theta, log = TRUE)},
               theta_proposal_sampler = function(theta) {theta+rnorm(1)}
)

## CPM : N=80, T=8192, p=1, rho=0.9963
before_time <- Sys.time()
rem80 <- run_mh(rem80, nsim = 10000)
after_time <- Sys.time()
print((after_time - before_time))


theta_chain_80 <- sapply(rem80$accept_chain, function(x) x$theta)
prop_theta_chain_80 <- sapply(rem80$proposed_chain, function(x) x$theta)
mean(theta_chain_80)
length(theta_chain_80)
dt <- data.table::data.table(prop_chain = prop_theta_chain_80, accept_chain = theta_chain_80)
dt <- dt[1:10000]
dt
data.table::fwrite(dt, file ="./output/output_chains_80.csv" )
plot(theta_chain_80, type='l')
points(prop_theta_chain_80, type='l', col='blue')





#### creating cpmmc object for N=30, T=8192, p=1, rho=0.9963 ####
rem30 <- cpmmc(data = rem_y,
               theta_0 = rnorm(1, 0, 3),
               u_0 = array(rnorm(30*1*8192), dim = c(30,1,8192)),
               rho = 0.9963,
               log_marginal_estimator_func = importance_estimator,
               log_theta_prior_density = function(x) {dnorm(x, 0, 3, log = TRUE)},
               log_theta_proposal_density = function(old_theta, new_theta) {dnorm(new_theta-old_theta, log = TRUE)},
               theta_proposal_sampler = function(theta) {theta+rnorm(1)}
)

## CPM : N=30, T=8192, p=1, rho=0.9963
before_time <- Sys.time()
rem30 <- run_mh(rem30, nsim = 10000)
after_time <- Sys.time()
print((after_time - before_time))
# Time difference of 31.21997 mins

theta_chain_30 <- sapply(rem30$accept_chain, function(x) x$theta)
prop_theta_chain_30 <- sapply(rem30$proposed_chain, function(x) x$theta)
mean(theta_chain_30) # 0.4723961
length(theta_chain_30) # 10001
dt30 <- data.table::data.table(prop_chain = prop_theta_chain_30, accept_chain = theta_chain_30)
dt30 <- dt[1:10000]
dt30
data.table::fwrite(dt, file ="./output/output_chains_30.csv" )
plot(theta_chain_30, type='l', ylim=c(-3,3))
points(prop_theta_chain_30, type='l', col='blue')



#### creating cpmmc object for N=50, T=8192, p=1, rho=0.9963 ####
rem50 <- cpmmc(data = rem_y,
               theta_0 = rnorm(1, 0, 2),
               u_0 = array(rnorm(50*1*8192), dim = c(50,1,8192)),
               rho = 0.9963,
               log_marginal_estimator_func = importance_estimator,
               log_theta_prior_density = function(x) {dnorm(x, 0, 2, log = TRUE)},
               log_theta_proposal_density = function(old_theta, new_theta) {dnorm(new_theta-old_theta, log = TRUE)},
               theta_proposal_sampler = function(theta) {theta+rnorm(1)}
)

## CPM : N=50, T=8192, p=1, rho=0.9963
before_time <- Sys.time()
rem50 <- run_mh(rem50, nsim = 10000)
after_time <- Sys.time()
print((after_time - before_time))
# Time difference of 31.21997 mins

theta_chain_50 <- sapply(rem50$accept_chain, function(x) x$theta)
prop_theta_chain_50 <- sapply(rem50$proposed_chain, function(x) x$theta)
mean(theta_chain_50) # 0.4723961
length(theta_chain_50) # 10001
dt50 <- data.table::data.table(prop_chain = prop_theta_chain_50, accept_chain = theta_chain_50)
dt50 <- dt[1:10000]
dt50
data.table::fwrite(dt, file ="./output/output_chains_50.csv" )
plot(theta_chain_30, type='l', ylim=c(-3,3))
points(prop_theta_chain_30, type='l', col='blue')



#### creating cpmmc object for N=5000, T=8192, p=1, rho=0 ####
rem5000_PH <- cpmmc(data = rem_y,
               theta_0 = rnorm(1, 0, 3),
               u_0 = array(rnorm(5000*1*8192), dim = c(5000,1,8192)),
               rho = 0,
               log_marginal_estimator_func = importance_estimator,
               log_theta_prior_density = function(x) {dnorm(x, 0, 3, log = TRUE)},
               log_theta_proposal_density = function(old_theta, new_theta) {dnorm(new_theta-old_theta, log = TRUE)},
               theta_proposal_sampler = function(theta) {theta+rnorm(1)}
)

## PM : N=5000, T=8192, p=1, rho=0.9963
before_time <- Sys.time()
rem5000_PH <- run_mh(rem5000_PH, nsim = 10000)
after_time <- Sys.time()
print((after_time - before_time))


theta_chain_5000_PH <- sapply(rem5000_PH$accept_chain, function(x) x$theta)
prop_theta_chain_5000_PH <- sapply(rem5000_PH$proposed_chain, function(x) x$theta)
mean(theta_chain_5000_PH)
length(theta_chain_5000_PH)
dt <- data.table::data.table(prop_chain = prop_theta_chain_5000_PH, accept_chain = theta_chain_5000_PH)
dt <- dt[1:10000]
dt
data.table::fwrite(dt, file ="./output/output_chains_5000_PH.csv" )
plot(theta_chain_5000_PH, type='l')
points(prop_theta_chain_5000_PH, type='l', col='blue')






#####################################################################



















