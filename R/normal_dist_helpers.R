

check_mean_sigma <- function(data, mean, sigma){
  if (is.matrix(data) & any(is.na(mean))){
    mean <- rep(1, ncol(data))
  } else if (any(is.na(mean))){
    mean <- 1
  }

  if (is.matrix(data) & any(is.na(sigma))){
    sigma <- diag(ncol(data))
  } else if (any(is.na(sigma))){
    sigma <- 1
  }

  list(mean=mean, sigma = sigma)
}

#' Helper function to perform idensity calculation for univariate and multivariate normal distribution
#'
#' Calculates density
#'
#' @param data, a array of data points, indexed first by the number of data points,
#' at each data point is the possibly multi-dimensional data
#' @param mean, mean of normal distribution
#' @param sigma, standard deviation (for univariate distribution), covariance matrix (for multivariate)
#' @return numeric value, density
normal_sampler <- function(n, mean=NA, sigma=NA, ...){

  mean_sigma <- check_mean_sigma(data, mean, sigma)


  if (!is.matrix(data)){
    return(rnorm(n, mean_sigma$mean, sd = mean_sigma$sigma, ...))
  } else {
    return(mvtnorm::rmvnorm(n, mean_sigma$mean, mean_sigma$sigma, ...))
  }

}

#' Helper function to generate samplesfor univariate and multivariate normal distribution
#'
#' Samples normal distribution
#'
#' @param n, number of data points to sample
#' at each data point is the possibly multi-dimensional data
#' @param mean, mean of normal distribution
#' @param sigma, standard deviation (for univariate distribution), covariance matrix (for multivariate)
#' @return numeric value, density
normal_density <- function(data, mean=NA, sigma=NA, ...){

  mean_sigma <- check_mean_sigma(data, mean, sigma)

  if (!is.matrix(data)){
    return(dnorm(data, mean_sigma$mean, sd = mean_sigma$sigma, ...))
  } else {
    return(mvtnorm::dmvnorm(data, mean_sigma$mean, mean_sigma$sigma, ...))
  }

}
