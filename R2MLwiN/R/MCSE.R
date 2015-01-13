#' Calculates the estimated Monte Carlo standard error (MCSE)
#' 
#' An internal function which calculates the estimated Monte Carlo standard
#' error (MCSE) for the posterior estimate of the mean, for use in \code{\link{sixway}}.
#' As MCMC is a simulation-based approach this induces (Monte Carlo) uncertainty
#' due to the random numbers it uses. This uncertainty reduces with more
#' iterations, and is measured by the MCSE.
#' 
#' @param chain Vector or \code{\link[coda]{mcmc}} object.
#' @param rho ACF for first lag.
#' @param ll Lower limit of x-axis, where value specified is multiplied by the length of the chain. Defaults to \code{0.5}.
#' @param ul Upper limit of x-axis, where value specified is multiplied by the length of the chain. Defaults to \code{20}.
#' 
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2014) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso
#' \code{\link{sixway}}
#'
# NOTE:
# var is variance of chain
# rho is acf for first lag
# runlength is the length of unthinned chain
MCSE <- function(chain, rho, ll = 0.5, ul = 20) {
  chain_var <- var(chain)
  if (is.mcmc(chain)) {
    runlength <- end(chain) - (start(chain) - 1)
  } else {
    runlength <- length(chain)
  }
  if (ul < ll) {
    temp <- ll
    ll <- ul
    ul <- temp
  }
  if (ul - ll < 1e-04) {
    ul <- 20
    ll <- 0.5
  }
  ll <- ll * runlength
  ul <- ul * runlength
  mult <- (sqrt((1 + rho)/(1 - rho))) * sqrt(chain_var)
  
  mcsepoints <- 1000
  mcse <- ll + ((ul - ll) * ((0:(mcsepoints - 1))/mcsepoints))
  updates <- matrix(mult, mcsepoints, 1)/sqrt(mcse)
  cbind(mcse, updates)
} 
