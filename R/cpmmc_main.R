




#' Instantiate cpmmc object and set initial parameters
#'
#' This is an S3 object constructor helper function for class cpmmc
#'
#' @param data observed data points
#' @param theta_0 initialisation of the vector of parameters of interest
#' @param u_0 initialisation of the auxiliary random variables
#' @param rho correlation parameter
#' @param log_marginal_estimator_func log likelihood estimator function
#' @param log_theta_prior_density log density function for the prior distribution of theta
#' @param log_theta_proposal_density log density function for theta proposals
#' @param theta_proposal_sampler sampling distribution for theta proposals
#' @return cpmmc object
#' @examples
#' cpmmc_obj <- cpmmc(data = rnorm(10, 1, 1),
#' theta_0 = 0,
#' u_0 = array(rnorm(5*10), dim = c(5,1,10)),
#' rho = 0.9,
#' log_marginal_estimator_func = function(x) {dnorm(x, 1, log=T)},
#' log_theta_prior_density = function(x) {dnorm(x, log = T)},
#' log_theta_proposal_density = function(old_theta, new_theta) {dnorm(new_theta-old_theta, log = T)},
#' theta_proposal_sampler = function(theta) {rnorm(1, mean = theta)}
#' )
#' @export
cpmmc <- function(data,
                  theta_0,
                  u_0,
                  rho,
                  log_marginal_estimator_func,
                  log_theta_prior_density,
                  log_theta_proposal_density,
                  theta_proposal_sampler){


  # define object structure
  obj <- list(

    # data
    data = data,

    # number of iterations
    N_ = dim(u_0)[1],

    # number of data points
    T_ = dim(u_0)[3],

    # number of parameters per observation
    p_ = dim(u_0)[2],

    #set chain length for future additions
    chain_length = 1,

    # accepted states in the markov chain
    chain = list(
      theta_0
    ),

    # latest state
    latest_state = list(theta = theta_0, u = u_0),

    # proposed states in the markov chain, empty list of lists, first list is the index i.e. 1, 2, 3
    # second list is the paramater at each iteration i.e. theta, u
    proposed_chain = list(
    ),

    # store rho for methods
    rho = rho,

    # importance sampler estimator (phat(y|theta,u))
    log_marginal_estimator_func = log_marginal_estimator_func,

    # theta proposal density, often a random walk proposal based on latest state (q density)
    log_theta_proposal_density = log_theta_proposal_density,

    # theta proposal sampler, often a random walk proposal based on latest state (q sampler)
    theta_proposal_sampler = theta_proposal_sampler,

    # theta prior density (p(theta))
    log_theta_prior_density = log_theta_prior_density
  )

  attr(obj,'class') <- c('cpmmc', 'metropolis_hastings', 'markov_chain') # #TODO inherit mh class, once implemented

  obj
}

#' S3 Implementation of single_mh_step generic method for cpmmc
#'
#' Runs Metropolis Hastings Algorithm to generate a new proposal and accepted state from
#'  the latest state of markov chain for cpmmc object, using intrinsic cpmmc proposals
#'
#' @param cpmmc object
#' @return object, with chains of parameters updated
#' @export
single_transition.cpmmc <- function(object){
  # get current state
  latest_state <- get_state(object)
  old_theta <- latest_state$theta
  old_u <- latest_state$u

  # sample new theta
  new_theta <- object$theta_proposal_sampler(old_theta)

  # sample new U
  rho <- object$rho

  dimension_single_observation <- object$p_
  number_ISsamples <- object$N_
  number_datapoints <- object$T_

  epsilon <- array(data =  rnorm(number_datapoints * number_ISsamples * dimension_single_observation),
                   dim = c(number_ISsamples, dimension_single_observation, number_datapoints))

  new_u <- rho * old_u + sqrt(1-rho^2) * epsilon

  proposal_param <- list(theta=new_theta, u=new_u)

  # calculate accept / reject probability using log likelihoods
  new_marginal_estimator <- object$log_marginal_estimator_func(object$data, new_theta, new_u)
  old_marginal_estimator <- object$log_marginal_estimator_func(object$data, old_theta, old_u)

  numerator <- new_marginal_estimator +
    object$log_theta_prior_density(new_theta) +
    object$log_theta_proposal_density(new_theta,old_theta)

  denominator <- old_marginal_estimator +
    object$log_theta_prior_density(old_theta) +
    object$log_theta_proposal_density(old_theta, new_theta)

  ar_prob <- numerator- denominator

  accept_bool <- log(runif(1)) < ar_prob
  if (accept_bool){
    accept_param <- proposal_param
  } else {
    accept_param <- latest_state
  }

  # set proposal and latest state within object
  object$latest_state <- accept_param
  object$chain_length <- object$chain_length + 1
  object$chain[[object$chain_length]] <- accept_param$theta
  object$proposed_chain[[object$chain_length-1]] <- proposal_param$theta

  object$log_new_marginal_estimator <- new_marginal_estimator
  object$log_old_marginal_estimator <- old_marginal_estimator

  # return latest params
  object
}







