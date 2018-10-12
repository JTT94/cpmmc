


#' Instantiate cpmmc object and set initial parameters
#'
#' This is an S3 object constructor helper function for class cpmmc
#'
#' @return cpmmc object
#' @export
cpmmc <- function(data,
                  theta_0,
                  u_0,
                  rho,
                  marginal_estimator_func,
                  theta_prior_density,
                  theta_proposal_density,
                  theta_proposal_sampler){


  # define object structure
  obj <- list(

    # data
    data = data,

    #set chain length for future additions
    chain_length = 1,

    # accepted states in the markov chain
    accept_chain = list(
      list(
        theta = theta_0,
        u = u_0
      )
    ),

    # proposed states in the markov chain, empty list of lists, first list is the index i.e. 1, 2, 3
    # second list is the paramater at each iteration i.e. theta, u
    proposed_chain = list(
      list()
    ),

    # store rho for methods
    rho = rho,

    # importance sampler estimator
    marginal_estimator_func = marginal_estimator_func,

    # theta proposal density, often a random walk proposal based on latest state
    theta_proposal_density = theta_proposal_density,

    # theta proposal sampler, often a random walk proposal based on latest state
    theta_proposal_sampler = theta_proposal_sampler,

    # theta prior density
    theta_prior_density = theta_prior_density
  )

  attr(obj,'class') <- 'cpmmc' # #TODO inherit mh class, once implemented

  obj
}

#' S3 Implementatio of get_state generic method for cpmmc
#'
#' Accesses the latest state of markov chain for cpmmc object
#'
#' @param cpmmc object
#' @return list, parameters of latest state
#' @export
get_state.cpmmc <- function(object){
  tail(object$accept_chain, n=1)[[1]]
}


#' S3 Implementatio of single_mh_step generic method for cpmmc
#'
#' Runs Metropolis Hastings Algorithm to generate a new proposal and accepted state from
#'  the latest state of markov chain for cpmmc object, using intrinsic cpmmc proposals
#'
#' @param cpmmc object
#' @return object, with chains of parameters updated
#' @export
single_mh_step.cpmmc <- function(object){
  # get current state
  latest_state <- get_state(object)
  old_theta <- latest_state$theta
  old_u <- latest_state$u

  # sample new theta
  new_theta <- object$theta_proposal_sampler(old_theta)

  # sample new U
  rho <- object$rho
  epsilon <- rnorm(length(old_u))
  new_u <- rho*old_u + sqrt(1-rho^2)* epsilon

  proposal_param <- list(theta=new_theta, u=new_u)

  # calculate accept / reject probability
  new_marginal_estimator <- object$marginal_estimator_func(object$data, new_theta, new_u)
  old_marginal_estimator <- object$marginal_estimator_func(object$data, old_theta, old_u)

  numerator <- new_marginal_estimator *
    object$theta_prior_density(new_theta) *
    object$theta_proposal_density(new_theta,old_theta)

  denominator <- old_marginal_estimator *
    object$theta_prior_density(old_theta) *
    object$theta_proposal_density(old_theta, new_theta)

  ar_prob <- numerator / denominator

  accept_bool <- runif(1) < ar_prob
  if (accept_bool){
    accept_param <- proposal_param
  } else {
    accept_param <- latest_state
  }

  # set proposal and latest state
  object$chain_length <- object$chain_length + 1
  object$accept_chain[[object$chain_length]] <- accept_param
  object$accept_chain[[object$chain_length-1]] <- proposal_param

  # return latest params
  object
}





