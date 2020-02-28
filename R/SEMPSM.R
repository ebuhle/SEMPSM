#' Fit Structural Equation Model of Pre-Spawning Mortality
#' 
#' Fit a hierarchical Bayesian structural equation model to data on pre-spawning mortality
#' and landscape and climatic variables using Stan.
#' 
#' @param psm A data frame with rows corresponding to observations, including columns named
#' \describe{
#' \item{\code{site}}{A factor giving site names.}
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
#' @param fit Stanfit object as returned by a previous call to [SEMPSM::SEMPSM()]. If
#' not NULL, initial values will be drawn randomly from the posterior samples in \code{fit}
#' using [SEMPSM::stan_init_cv()]; otherwise they will be generated using [SEMPSM::stan_init()].
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
#' @param pars A character vector specifying parameters of interest to be saved. 
#' @param chains A positive integer specifying the number of Markov chains.
#' @param iter A positive integer specifying the number of iterations for each chain (including warmup).
#' @param warmup A positive integer specifying the number of warmup (aka burnin) iterations per chain.
#' @param thin A positive integer specifying the period for saving samples.
#' @param ... Other arguments to be passed to [rstan::sampling()].
#' 
#' @return An object of S4 class [rstan::stanfit].
#' 
#' @export

SEMPSM <- function(psm, X, normal_indx, gamma_indx, L = 1, fit = NULL,
                   I0_Z = 1, I_su = 1, I_su_Z = 1, I_fa = 1, I_fa_Z = 1, 
                   I_fit = rep(1, nrow(psm)), I_lpd = rep(1, nrow(psm)),
                   pars = c("a0","A","Z","phi","g_mu_X",
                            "mu_b0","b0_Z","sigma_b0","b0",
                            "mu_b_su","b_su_Z","sigma_b_su","b_su",
                            "mu_b_fa","b_fa_Z","sigma_b_fa","b_fa",
                            "sigma_psm","p_psm","ll_psm"),
                   chains = 3, iter = 12000, warmup = 2000, thin = 5, ...) 
{
  stan_dat <- stan_data(psm, X, normal_indx, gamma_indx, L = L,
                        I0_Z = I0_Z, I_su = I_su, I_su_Z = I_su_Z, I_fa = I_fa, I_fa_Z = I_fa_Z, 
                        I_fit = I_fit, I_lpd = I_lpd)
  
  out <- rstan::sampling(stanmodels$SEMPSM,
                         data = stan_dat, 
                         init = if(is.null(fit)) {
                           lapply(1:3, function(i) stan_init(stan_dat))
                         } else {
                           lapply(1:3, function(i) stan_init_cv(fit))
                         },
                         pars = pars, chains = chains, iter = iter, warmup = warmup, thin = thin)
}
