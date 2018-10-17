

#' Instantiate metropolis_hastings object and set initial parameters
#'
#' This is an S3 object constructor helper function for class metropolis_hastings which inherit from markov_chain class
#'
#' @param initial_params, numeric for initial state of chain, list for multiple named parameters
#' @param log_target_density, the log target density for the MH algorithm, i.e. the log density for the stationary distribution of the Markov Chain
#' @param log_proposal_density, log density function for parameter proposals
#' @param proposal_sampler, distribution function to sample new parameter proposals
#' @return metropolis_hastings object
#' @export

metropolis_hastings <- function(initial_params, log_target_density, log_proposal_density, proposal_sampler){

  # acceptance probability
  log_acceptance_probability <- function(current_state, proposal_state){
    accept_reject_prob <- log_target_density(proposal_state) +
      log_proposal_density(proposal_state, current_state) -
      log_target_density(current_state) -
      log_proposal_density(current_state,proposal_state)
    return(accept_reject_prob)
  }

  # transition sampler
  transition_sampler <- function(current_state){

    # simulate new state
    proposal_state <- proposal_sampler(current_state)

    # accept / reject
    accept_reject_prob <- log_acceptance_probability(current_state, proposal_state)

    ar <-  log(runif(1)) < accept_reject_prob

    if (ar) {
      return(proposal_state)
    } else {
      return(current_state)
    }
  }

  obj <- markov_chain(initial_value = initial_params, transition_kernel=transition_sampler)
  obj$log_target_density <- log_target_density
  obj$log_proposal_density <- log_proposal_density
  obj$proposal_sampler <- proposal_sampler

  # set class, inherit cpmmc
  class(obj) <- c("metropolis_hastings", "markov_chain")

  obj

}

