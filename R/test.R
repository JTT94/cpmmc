##' get minimum of maxima of a set of vectors
##'
##' @param ... numeric vectors
minimax = function(...) {
  args = list(...)
  maxs = sapply(args, max)
  return(min(maxs))
}
