#' Plots MCMC chain trajectories
#'
#' This function draws trajectories of MCMC chains.
#'
#' @param object A data frame of parameter chains from MLwiN.
#' @param Range An integer vector of length two specifying the first and last
#' iterations of the chains.
#' @param selected A character vector specifying the selected chains to be
#' plotted.
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2015) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso \code{\link{sixway}}
#'
#' @examples
#'
#' \dontrun{
#' library(R2MLwiN)
#' #NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#' #...so please change relevant line if different
#' #if using R2MLwiN via WINE, the path may look like
#' #options(MLwiN_path = '/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/')
#'
#' ## Example: tutorial
#' data(tutorial)
#' formula = normexam ~ (0|cons + standlrt) + (2|cons + standlrt) + (1|cons)
#' levID = c('school', 'student')
#' estoptions = list(EstM = 1)
#' mymodel = runMLwiN(formula, levID, indata = tutorial, estoptions = estoptions)
#'
#' trajectories(mymodel, Range = c(4501, 5000))
#' }
#'
#' @export
trajectories <- function(object, Range = c(1, 5000), selected = NULL) {
  # This function draws trajectories of the chains for each parameter estimate
  
  if (class(object) == "mlwinfitMCMC") {
    chains <- object@chains
  } else {
    if (is.mcmc(object) || is.mcmc.list(object)) {
      chains <- object
    } else {
      chains <- mcmc(object)
    }
  }
  
  if (is.null(selected)) {
    selected <- varnames(chains)
  }
  
  chains <- window(chains, Range[1], Range[2])
  
  if (nvar(chains) == 1) 
    opar <- par(mfrow = c(1, 1))
  if (nvar(chains) == 2) 
    opar <- par(mfrow = c(2, 1))
  if (nvar(chains) == 3) 
    opar <- par(mfrow = c(3, 1))
  if (nvar(chains) == 4) 
    opar <- par(mfrow = c(2, 2))
  if (nvar(chains) > 4) 
    opar <- par(mfrow = c(3, 2))
  if (nvar(chains) > 6) 
    opar <- par(mfrow = c(3, 3))
  
  nwindows <- 0
  
  for (param in varnames(chains)) {
    if (is.mcmc(chains)) {
      plot(rownames(chains), chains[, param], xlab = "iteration", ylab = param, type = "l")
    } else {
      ymin <- min(unlist(chains[1:niter(chains), param]))
      ymax <- max(unlist(chains[1:niter(chains), param]))
      plot(rownames(chains[[1]]), chains[[1]][, param], xlab = "iteration", ylab = param, type = "l", ylim = c(ymin, 
                                                                                                               ymax), col = 1)
      for (j in 2:nchain(chains)) {
        lines(rownames(chains[[j]]), chains[[j]][, param], col = j)
      }
    }
    nwindows <- nwindows + 1
    if ((nwindows%%9) == 0) {
      dev.new()
      opar <- par(mfrow = c(3, 3))
    }
  }
  on.exit(par(opar))
} 
