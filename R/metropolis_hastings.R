




#' Generic function to run single Metropolis Hastings Step
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list, proposal state and accepted state
single_mh_step <- function(object) {
  UseMethod('single_mh_step')
}


#' Generic function to run Metropolis Hastings algorithm
#'
#' @param object, markov chain object e.g. cpmmc
#' @param N, number of samples from posterior
#' @return list, proposal state and accepted state
run_mh <- function(object, nsim) {
  UseMethod('run_mh')
}


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

