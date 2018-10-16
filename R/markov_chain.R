#' Generic function to access latest state in a markov chain
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list of parameters of latest accepted state
get_state <- function(object){
  UseMethod('get_state')
}


#' Generic function to access proposed and/or accepted chains from output of run_mh() function
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list of parameters of latest accepted state
get_chain <- function(object, chain) {
  UseMethod('get_chain')
}

#' Generic function to access proposed and/or accepted chains from output of run_mh() function
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list of parameters of latest accepted state
get_chain_length <- function(object) {
  UseMethod('get_chain_length')
}

#' Generic function to extend the markov chain by a single step
#'
#' @param object, markov chain object e.g. cpmmc
#' @return object, markkov chain object with MC extended
single_transition <- function(object) {
  UseMethod('single_transition')
}

#' Generic function to extend the markov chain by a single step
#'
#' @param object, markov chain object e.g. cpmmc
#' @return object, markkov chain object with MC extended by n steps
run_chain <- function(object, chain_length) {
  UseMethod('run_chain')
}


#' Instantiate Markov chain
#'
#' This is an S3 object constructor helper function for class random_effects_model which inherics from cpmmc class
#'
#' @param intial_value, numeric for initial state of chain, list for multiple named params
#' @param transition_sampler, function, takes current state and returns next state in chain
#' @return markov_chain object
#' @export
markov_chain <- function(intial_value, transition_kernel){

  # instantiate MC object
  obj <- list(

    # markov chain, list of states
    chain = list(
      intial_value
    ),

    # kernel to determine next state
    transition_kernel = transition_kernel,

    # number of elments of chain
    chain_length = 1
  )

  # set class, inherit cpmmc
  attr(obj,'class') <- "markov_chain"

  # return object
  obj

}

get_chain.markov_chain <- function(object){
  object$chain
}

get_state.markov_chain <- function(object){
  tail(object$chain, n=1)[[1]]
}

single_transition.markov_chain <- function(object){
  current_state <- get_state(object)

  # get next state
  next_state <- object$transition_kernel(current_state)

  # set proposal and latest state within object
  object$chain_length <- object$chain_length + 1
  object$chain[[object$chain_length]] <- next_state

  # return object
  object
}

get_chain_length.markov_chain <- function(object){
  object$chain_length
}

run_chain.markov_chain <- function(object, nsim){
  for (i in 1:nsim){
    object <- single_jump(object)
  }

  object
}
