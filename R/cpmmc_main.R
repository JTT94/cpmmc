


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

    # number of iterations
    N_ = length(u_0[[1]]),

    # number of data points
    T_ = length(u_0),

    # number of parameters per observation
    p_ = length(u_0[[1]][[1]]),

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

    # importance sampler estimator (phat(y|theta,u))
    marginal_estimator_func = marginal_estimator_func,

    # theta proposal density, often a random walk proposal based on latest state (q density)
    theta_proposal_density = theta_proposal_density,

    # theta proposal sampler, often a random walk proposal based on latest state (q sampler)
    theta_proposal_sampler = theta_proposal_sampler,

    # theta prior density (p(theta))
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

  dimension_single_observation <- object$p_
  number_ISsamples <- object$N_
  number_datapoints <- object$T_

  epsilon <- array(data =  rnorm(number_datapoints * number_ISsamples * dimension_single_observation),
                   dim = c(number_datapoints, dimension_single_observation, number_ISsamples),
                   dimnames = NULL)

  new_u <- rho * old_u + sqrt(1-rho^2) * epsilon


  proposal_param <- list(theta=new_theta, u=new_u)

  # calculate accept / reject probability using log likelihoods
  new_marginal_estimator <- object$marginal_estimator_func(object$data, new_theta, new_u)
  old_marginal_estimator <- object$marginal_estimator_func(object$data, old_theta, old_u)

  numerator <- new_marginal_estimator +
    object$theta_prior_density(new_theta) +
    object$theta_proposal_density(new_theta,old_theta)

  denominator <- old_marginal_estimator +
    object$theta_prior_density(old_theta) +
    object$theta_proposal_density(old_theta, new_theta)

  ar_prob <- numerator - denominator

  accept_bool <- log(runif(1)) < ar_prob
  if (accept_bool){
    accept_param <- proposal_param
  } else {
    accept_param <- latest_state
  }

  # set proposal and latest state within object
  object$chain_length <- object$chain_length + 1
  object$accept_chain[[object$chain_length]] <- accept_param
  object$proposed_chain[[object$chain_length-1]] <- proposal_param

  # return latest params
  object
}



#' S3 Implementatio of run_mh generic method for cpmmc
#'
#' Runs Metropolis Hastings Algorithm to generate N new proposals and accepted states from
#'  the latest state of markov chain for cpmmc object, using intrinsic cpmmc proposals
#'
#' @param cpmmc object
#' @param N number of samples from posterior
#' @return object, with chains of parameters updated
#' @export
run_mh.cpmmc <- function(object, nsim) {
  # iterate through MH steps
  for (i in seq_len(nsim)) {
    object <- single_mh_step(object)
  }

  # return latest params
  object
}



#' S3 Implementatio of get_chain generic method for cpmmc
#'
#' Accesses the proposed and/or accepted chains for cpmmc object
#'
#' @param cpmmc object
#' @param chain type of chain wanted from "proposed", "accepted", "theta", "u", "proposed theta", "proposed u", "accepted theta", "accepted u"
#' @return chain="proposed" returns a list the proposed chain of theta and u
#' @return chain="accepted" returns a list the accepted chain of theta and u
#' @return chain="proposed theta" a list returns the proposed chain of theta
#' @return chain="proposed u" returns a list the proposed chain of u
#' @return chain="accepted theta" returns a list the accepted chain of theta
#' @return chain="accepted u" returns a list the accepted chain of u
#' @export
get_chain.cpmmc <- function(object, chain) {
  if (chain %in% c("proposed", "accepted", "theta", "u", "proposed theta", "proposed u", "accepted theta", "accepted u")) {
    if (chain == "proposed") {
      return(object$proposed_chain)
    }
    if (chain == "accepted") {
      return(object$accept_chain)
    }
    if (chain == "proposed theta") {
      return(as.list(sapply(object$proposed_chain, function(x) x$theta[[1]])))
    }
    if (chain == "proposed u") {
      return(as.list(sapply(object$proposed_chain, function(x) x$u[[1]])))
    }
    if (chain == "accepted theta") {
      return(as.list(sapply(object$accept_chain, function(x) x$theta[[1]])))
    }
    if (chain == "accepted u") {
      return(as.list(sapply(object$accept_chain, function(x) x$u[[1]])))
    }
  } else {
    stop("The chain wanted is not available.")
  }
}


