#' Assemble Data for Stan
#' 
#' Assemble list of data to be passed to [rstan::stan()].
#'
#' @param psm A data frame with rows corresponding to observations, including columns named
#' \describe{
#' \item{\code{site}}{A factor or character vector giving site names. If a character vector,
#' it will be coerced to factor.}
#' \item{\code{ppt_su}}{A numeric variable giving summer precipitation in mm.}
#' \item{\code{ppt_fa}}{A numeric variable giving fall precipitation in mm.}
#' \item{\code{n}}{An integer variable giving the number of carcasss sampled.}
#' \item{\code{n_psm}}{An integer variable giving the number of pre-spawning mortalities
#' in each sample.}
#' }
#' @param X A matrix with rows corresponding to sites and columns corresponding to 
#' landscape variables, to be modeled as either normal- or gamma-distributed.
#' @param normal_indx An integer vector giving indices of columns of \code{X} to be modeled
#' as normally distributed.
#' @param gamma_indx An integer vector giving indices of columns of \code{X} to be modeled as
#' gamma-distributed.
#' @param L Integer giving the number of latent landscape factors to fit.
#' @param I0_Z Integer (0/1) indicating whether to include main effects of latent landscape 
#' factors on pre-spawning mortality risk.
#' @param I_su Integer (0/1) indicating whether to include a main effect of summer precipitation
#' on pre-spawning mortality risk.
#' @param I_su_Z Integer (0/1) indicating whether to include interactions between 
#' summer precipitation and latent landscape factors on pre-spawning mortality risk.
#' @param I_fa Integer (0/1) indicating whether to include a main effect of fall precipitation
#' on pre-spawning mortality risk.
#' @param I_fa_Z Integer (0/1) indicating whether to include interactions between 
#' fall precipitation and latent landscape factors on pre-spawning mortality risk.
#' @param I_fit Integer vector (0/1) of length \code{nrow(psm)} indicating whether each
#' pre-spawning mortality observation should be included in the likelihood. This may be
#' useful for fitting some observations while simulating from the posterior predictive
#' distribution for others.
#' @param I_lpd Integer vector (0/1) of length \code{nrow(psm)} indicating whether
#' to evaluate the log posterior predictive density for each observation.
#' 
#' @return A named list of data to be passed to [SEMPSM::sem_psm()].
#' 
#' @export

stan_data <- function(psm, X, normal_indx, gamma_indx, L = 1,
                      I0_Z = 1, I_su = 1, I_su_Z = 1, I_fa = 1, I_fa_Z = 1, 
                      I_fit = rep(1, nrow(psm)), I_lpd = rep(1, nrow(psm)))
{
  list(S = nrow(X), 
       D_normal = length(normal_indx), D_gamma = length(gamma_indx),
       X = X, 
       L = L,
       N = nrow(psm), 
       site = as.numeric(factor(psm$site)),
       ppt_su = array(as.vector(scale(psm$ppt_su/10, scale = FALSE)), dim = nrow(psm)),
       ppt_fa = array(as.vector(scale(psm$ppt_fa/10, scale = FALSE)), dim = nrow(psm)),
       I0_Z = I0_Z,
       I_su = I_su,
       I_su_Z = I_su_Z,
       I_fa = I_fa,
       I_fa_Z = I_fa_Z,
       n = psm$n,
       n_psm = psm$n_psm,
       I_fit = I_fit,
       I_lpd = I_lpd)
}