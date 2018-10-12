cpmmc_obj <- cpmmc(data = c(1,2,3),
                   theta_0 = 0,
                   u_0 = 1,
                   rho = 0.9,
                   marginal_estimator_func = function(data, new_theta, new_u) rnorm(1),
                   theta_prior_density = function(x) dnorm(x),
                   theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta),
                   theta_proposal_sampler = function(theta) rnorm(1)
)

object <- cpmmc_obj



for(i in 1:5000){
  cpmmc_obj <- single_mh_step(cpmmc_obj)
  print(length(cpmmc_obj$accept_chain))
}

sapply(cpmmc_obj$accept_chain, function(x) x$u[[1]])


func <- function(x) x
func <- function(x){
  x
}

func <- function(x){
  return(x)
}
