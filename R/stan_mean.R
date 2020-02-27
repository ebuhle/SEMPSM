#' Extract Posterior Mean Across All Chains From Stanfit Object
#' 
#' Wrapper for [rstan::get_posterior_mean()] making it less cumbersome to extract 
#' the mean of a parameter (or other quantity) across all chains.
#' 
#' @param object An object of class stanfit.
#' @param pars Character string providing the parameter (or other quantity) name of interest.
#' @param ... Additional arguments to be passed to [rstan::get_posterior_mean()].
#' 
#' @return When permuted = TRUE (the default), this function returns an array 
#' representing samples for \code{par} with all chains merged together.
#' 
#' @export

stan_mean <- function(object, pars)
{
  mm <- get_posterior_mean(object, pars)
  return(mm[,ncol(mm)])
}
