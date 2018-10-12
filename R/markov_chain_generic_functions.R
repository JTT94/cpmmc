

#' Generic function to access latest state in a markov chain
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list of parameters of latest accepted state
get_state <- function(object){
  UseMethod('get_state')
}


#' Generic function to run single Metropolis Hastings Step
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list, proposal state and accepted state
single_mh_step <- function(object){
  UseMethod('single_mh_step')
}

