

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


#' Generic function to run Metropolis Hastings algorithm
#'
#' @param object, markov chain object e.g. cpmmc
#' @param N, number of samples from posterior
#' @return list, proposal state and accepted state
run_mh <- function(object, N) {
  UseMethod('run_mh')
}


#' Generic function to access proposed and/or accepted chains from output of run_mh() function
#'
#' @param object, markov chain object e.g. cpmmc
#' @return list of parameters of latest accepted state
get_chain <- function(object, chain){
  UseMethod('get_chain')
}




