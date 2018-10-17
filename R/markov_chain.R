#' Generic function to access latest state in a markov chain
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return list of parameters of the latest accepted state
#' @export
get_state <- function(object){
  UseMethod('get_state')
}


#' Generic function to access proposed and/or accepted chains from output of run_chain() function
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return list of parameters of latest accepted state
#' @export
get_chain <- function(object, chain) {
  UseMethod('get_chain')
}

#' Generic function to access proposed and/or accepted chains from output of run_chain() function
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return numeric value for the length of the Markov chain
#' @export
get_chain_length <- function(object) {
  UseMethod('get_chain_length')
}

#' Generic function to extend the markov chain by a single step
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return object, markov_chain object with MC extended by 1 steps using MCMC algorithms specified by the class of the object
#' @export
single_transition <- function(object) {
  UseMethod('single_transition')
}

#' Generic function to extend the markov chain by n steps
#'
#' @param object, markov chain object e.g. cpmmc, metropolis_hastings
#' @param chain_length, number of simulations for the MCMC algorithm to run
#' @return object, markov chain object with MC extended by n steps using MCMC algorithms corresponding to the class of the object
#' @export
run_chain <- function(object, chain_length) {
  UseMethod('run_chain')
}


#' Instantiate Markov chain object and set the transition kernel
#'
#' This is an S3 object constructor helper function for class markov_chain
#'
#' @param intial_value, numeric for initial state of chain, list for multiple named parameters
#' @param transition_kernel, function for the transition kernel of the Markov process, which takes the current state and returns the next state of the process
#' @return markov_chain object
#' @export
markov_chain <- function(initial_value, transition_kernel){

  # instantiate MC object
  obj <- list(

    # markov chain, list of states
    chain = list(
      initial_value
    ),

    # latest state
    latest_state = initial_value,

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


#' S3 Implementation of single_transition generic method for markov_chain
#'
#' Generate the next state of the Markov chain using the transition kernel
#'
#' @param object, markov_chain object
#' @return markov_chain object, with chains of parameters updated by one step using the transition kernel
#' @export

single_transition.markov_chain <- function(object){
  current_state <- get_state(object)

  # get next state
  next_state <- object$transition_kernel(current_state)

  # set proposal and latest state within object
  object$latest_state <- next_state
  object$chain_length <- object$chain_length + 1
  object$chain[[object$chain_length]] <- next_state

  # return object
  object
}


#' S3 Implementation of run_chain generic method for markov_chain
#'
#' Generate a Markov chain of required length using the transition kernel of the markov_chain object
#'
#' @param object, markov_chain object
#' @param chain_length, number of new states generated using the transition kernel
#' @return markov_chain object, with chains of parameters updated according to the length of chain
#' @export
run_chain.markov_chain <- function(object, chain_length){
  for (i in seq_len(chain_length)){
    object <- single_transition(object)
  }
  object
}



#' S3 Implementation of get_chain generic method for markov_chain
#'
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return list of parameters of latest accepted state
#' @export
get_chain.markov_chain <- function(object){
  object$chain
}


#' S3 Implementation of get_state generic method for markov_chain
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return list of parameters of the latest accepted state
#' @export
get_state.markov_chain <- function(object){
  object$latest_state
}

#' S3 Implementation of get_chain_length generic method for markov_chain
#'
#' @param object, markov_chain object e.g. cpmmc, metropolis_hastings
#' @return numeric value for the length of the Markov chain
#' @export
get_chain_length.markov_chain <- function(object){
  object$chain_length
}
