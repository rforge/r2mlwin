# NOTE:
# var is variance of chain
# rho is acf for first lag
# runlength is the length of unthinned chain
MCSE <- function(chain, rho, ll=0.5, ul=20) {
  chain_var <- var(chain)
  if (is.mcmc(chain)){
    runlength <- end(chain)-(start(chain)-1)
  }
  else{
    runlength <- length(chain)
  }
  if (ul < ll) {
    temp <- ll
    ll <- ul
    ul <- temp
  }
  if (ul - ll < 0.0001) {
    ul = 20
    ll = 0.5
  }
  ll <- ll * runlength
  ul <- ul * runlength
  mult <- (sqrt((1.0 + rho) / (1.0 - rho))) * sqrt(chain_var)
  
  mcsepoints <- 1000
  mcse <- ll + ((ul - ll) * ((0:(mcsepoints - 1)) / mcsepoints))
  updates <- matrix(mult, mcsepoints, 1) / sqrt(mcse)
  cbind(mcse, updates)
}