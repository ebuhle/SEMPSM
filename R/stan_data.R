#' Assemble Data for Stan
#' 
#' Assemble list of data to be passed to [rstan::stan()].
#'
#' @param psm A data frame with rows corresponding to observations, including columns named
#' \code{}

stan_data <- function(psm, X, normal_indx, gamma_indx, L = 1,
                      I0_Z = 1, I_su = 1, I_su_Z = 1, I_fa = 1, I_fa_Z = 1, 
                      I_fit = rep(1, nrow(psm)), I_lpd = rep(1, nrow(psm)))
{
  list(S = nrow(X), 
       D_normal = length(normal_indx), D_gamma = length(gamma_indx),
       X = X, 
       L = L,
       N = nrow(psm), 
       site = as.numeric(psm$site),
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