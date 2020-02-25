#' Extract a Single Parameter From a Stanfit Object
#' 
#' Wrapper for \code{rstan::extract} making it less cumbersome to extract a single parameter.
#' 
#' @param object An object of class stanfit.
#' @param par Character string providing the parameter (or other quantity) name of interest.
#' @param ... Additional arguments to be passed to [rstan::extract].
#' 
#' @return When permuted = TRUE (the default), this function returns an array 
#' representing samples for \code{par} with all chains merged together.
#' 
#' @export

extract1 <- function(object, par, ...)
{
  extract(object, par, ...)[[1]]
}
