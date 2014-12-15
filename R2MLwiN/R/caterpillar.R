#' Draws a caterpillar plot (in MLwiN style).
#' 
#' A convenient wrapper for the \code{\link[plot]{plot}} function with the addition
#' of error bars, e.g. to create caterpillar plots.
#' 
#' @param y A numerical vector specifying the \code{y} coordinates
#' (e.g. the means or medians); see \code{\link[plot.default]{plot.default}}.
#' @param x A numerical vector specifying the \code{x} coordinates; see \code{\link[plot.default]{plot.default}}.
#' @param qtlow A numerical vector (e.g. of lower-quantiles) to be used to plot lower error
#' bars.
#' @param qtup A numerical vector (e.g. of upper-quantiles) to be used to upper plot error
#' bars.
#' @param xlab A label for the \code{x} axis. This is empty by default; see \code{\link[plot.default]{plot.default}}.
#' @param ylab A label for the \code{y} axis. This is empty by default; see \code{\link[plot.default]{plot.default}}.
#' @param xlim The \code{x} limits \code{(x1, x2)} of the plot. Note that
#' \code{x1 > x2} is allowed and leads to a `reversed axis'. The default value,
#' \code{NULL}, indicates that the range of the finite values to be plotted
#' should be used; see \code{\link[plot.default]{plot.default}}.
#' @param ylim The y limits of the plot; see \code{\link[plot.default]{plot.default}}.
#' @param main A main title for the plot; see \code{\link[plot.default]{plot.default}}.
#' 
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2014) Centre for Multilevel Modelling, University of Bristol, U.K.
#' 
#' @seealso \code{\link{caterpillarR}}
#' 
#' @examples
#'  
#' \dontrun{
#' library(R2MLwiN)
#' # NOTE: if MLwiN not saved where R2MLwiN defaults to:
#' # options(MLwiN_path="path/to/MLwiN vX.XX/")
#' # If using R2MLwiN via WINE, the path may look like:
#' # options(MLwiN_path = "/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN vX.XX/") 
#' 
#' # Example using tutorial dataset
#' data(tutorial, package = "R2MLwiN")
#' (mymodel <- runMLwiN(normexam ~ 1 + standlrt + (school|1) + (student|1),
#'                      estoptions = list(EstM = 1, resi.store = TRUE,
#'                      resi.store.levs = 2, mcmcMeth = list(iterations = 5001)),
#'                      data = tutorial))
#' 
#' resi.chain2 = mymodel["resi.chains"]$resi_lev2
#' 
#' # For each iteration, rank the schools
#' u0rank = apply(resi.chain2, 1, rank)
#' # For each school, calculate the mean rank...
#' u0rankmn = apply(u0rank, 1, mean)
#' u0ranklo = apply(u0rank, 1, function(x) quantile(x, .025))
#' u0rankmd = apply(u0rank, 1, median)
#' u0rankhi = apply(u0rank, 1, function(x) quantile(x, .975))
#' rankno = order(u0rankmn)
#' 
#' caterpillar(y = u0rankmn[rankno], x = 1:65, qtlow = u0ranklo[rankno],
#'             qtup = u0rankhi[rankno], xlab = "School", ylab = "Rank")
#' }
#' 
#' @export
caterpillar = function(y, x, qtlow, qtup, xlab="",ylab="", xlim=NULL,ylim=NULL,main=""){
  #This function draws a caterpillar plot
  
  plot(x,y,xlim=xlim,ylim=ylim,pch=15, xlab=xlab,ylab=ylab,main=main)
  points(x,qtlow,pch=24,bg="grey")
  points(x,qtup,pch=25,bg="grey")
  for(i in 1:length(x)) {
    lines(rep(x[i],2),c(qtlow[i],qtup[i]))
  }
}
