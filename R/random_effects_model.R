

#' Helper function to perform importance sampling for the random effect model
#'
#' Runs importance sampling
#'
#' @param data, a array of data points, indexed first by the number of data points,
#' at each data point is the possibly multi-dimensional data
#' @param theta, a possibly multivariate array constant added to each 2-dimensional entry of u_t
#' must be same number of parameters as each entry of u_t and of each data-point
#' @param new_u, a 3-dimensional array of auxilliary variables used for proposal in importance sampling
#' dim = c(number_of_samples, dimension_of_each_point, number_of_data_points)
#' @return numeric value, log of importance sampling estimate for g_theta = N(x,1), f_theta = N(theta,1)
normal_log_IS_estimate <- function(data, theta, new_u){
  p_hats <- sapply(seq_along(data), function(i) {
    sum(dnorm(data[i], mean = new_u[,,i] + theta)) / length(new_u[,,i])})

  sum(log(p_hats))
}

#' Instantiate random_effects_model object
#'
#' This is an S3 object constructor helper function for class random_effects_model which inherics from cpmmc class
#'
#' @return random_effects_model object
#' @export
normal_random_effect_model <- function(data, theta_0, u_0, rho){

  # instantiate cpmmc object with normal densities, proposals and IS estimator
  obj <- cpmmc(data = data,
                     theta_0 = theta_0,
                     u_0 = u_0,
                     rho = rho,
                     log_marginal_estimator_func = normal_log_IS_estimate,
                     log_theta_prior_density = function(x) dnorm(x, log = T),
                     log_theta_proposal_density = function(old_theta, new_theta) dnorm(new_theta-old_theta, log = T),
                     theta_proposal_sampler = function(theta) rnorm(1, mean = theta)
  )

  # set class, inherit cpmmc
  attr(obj,'class') <- c('cpmmc', 'metropolis_hastings', 'markov_chain')

  # return object
  obj

}
