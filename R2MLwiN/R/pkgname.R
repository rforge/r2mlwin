#' Running MLwiN from within R
#' 
#' R2MLwiN is an R command interface to the MLwiN multilevel modelling
#' software package, allowing users to fit multilevel models using
#' MLwiN (and also WinBUGS / OpenBUGS) from within the R environment.
#'
#' @importFrom rbugs runBugs rbugs2coda genBugsScript getBugsOutput rbugs
#' @importFrom coda mcmc mcmc.list as.mcmc.list read.coda effectiveSize raftery.diag thin is.mcmc
#' @importFrom lattice histogram densityplot xyplot qqmath Rows trellis.par.get trellis.par.set panel.xyplot panel.grid panel.abline panel.segments
#' @importFrom foreign read.dta write.dta
#' @importFrom digest digest
#' @importFrom methods show
#' @importFrom stats4 coef logLik summary vcov update
#' @importFrom stats formula
#' @importFrom Matrix nnzero sparseMatrix
#' 
#' @section Important differences between version 0.8-0 and earlier versions:
#' A number of wide-ranging changes, including a new model-fitting syntax more
#' in keeping with that conventionally used in R, were introduced in
#' \pkg{R2MLwiN} version 0.8-0.
#' 
#' The demos, which replicate both the User's Guide to MLwiN (Rasbash et al, 2012) and
#' MCMC Estimation in MLwiN (Browne, 2012) manuals, provide practical demonstrations of many
#' of these changes. See \code{demo(package = "R2MLwiN")} for a list of demo titles; to run one
#' type e.g. \code{demo(UserGuide03)} or view a demo's script via
#' \code{file.show(system.file("demo", "UserGuide03", package = "R2MLwiN"))}.
#' 
#' \itemize{
#'
#'  \item{The Formula is now specified via a \code{\link[stats]{formula}} object. So, for example,
#'  previously a 2-level model random intercept model would be specified by e.g.
#'  \code{normexam ~ (0|cons + standlrt) + (2|cons) + (1|cons), levID = c('school', 'student')}
#'  (with \code{normexam} the response variable, \code{cons} a constant of ones forming the intercept,
#'  which is allowed to vary at level 1 (\code{student}) and level 2 (\code{school}), and
#'  \code{standlrt} included as a predictor in the fixed part of the model). Whilst with back-compatibility
#'  is preserved (i.e. this specification will currently still work) the same model can now be more
#'  parsimoniously specified via \code{normexam ~ 1 + standlrt + (school|1) + (student|1)}.
#'  As well examples in the demos, see also \code{\link{runMLwiN}} and \code{\link{Formula.translate}} for further info.}
#'  
#'  \item{\code{\link{df2matrix}} and \code{\link{matrix2df}} functions have been added to convert multiple membership
#'  information between \code{\link[base]{data.frame}} and \code{\link[base]{matrix}} formats.}
#' }
#'
#' @examples
#' \dontrun{
#' library(R2MLwiN)
#' ## Modify the following paths as appropriate.
#' ## MLwiN folder
#' mlwin ='C:/Program Files (x86)/MLwiN v2.29/'
#' ## MLwiN sample worksheet folder
#' wspath=paste(mlwin,'/samples/',sep='')
#' 
#' ## MLwiN sample worksheet: tutorial dataset
#' wsfile=paste(wspath,'tutorial.ws',sep='');inputfile=paste(tempdir(),'/tutorial.dta',sep='')
#' ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#' library(foreign);indata =read.dta(inputfile)
#' 
#' ## Define the model
#' formula=normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)
#' levID=c('school','student')
#' ## Choose option(s) for inference
#' estoptions= list(EstM=1)
#' ## Fit the model
#' (mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions, MLwiNPath=mlwin))
#' 
#' ## The R2MLwiN package includes scripts to replicate all the analyses in
#' ## Browne, W.J. (2009) MCMC estimation in MLwiN Version 2.13.
#' ## Version 2.27 is available online; download from the following link:
#' ## http://www.bristol.ac.uk/cmm/software/mlwin/download/mcmc-print.pdf
#' ## Centre for Multilevel Modelling, University of Bristol
#' 
#' #Contents
#' #01 Introduction to MCMC Estimation and Bayesian Modelling
#' #02 Single Level Normal Response Modelling
#' #03 Variance Components Models
#' #04 Other Features of Variance Components Models
#' #05 Prior Distributions, Starting Values and Random Number Seeds
#' #06 Random Slopes Regression Models
#' #07 Using the WinBUGS Interface in MLwiN
#' #08 Running a Simulation Study in MLwiN
#' #09 Modelling Complex Variance at Level 1 / Heteroscedasticity
#' #10 Modelling Binary Responses
#' #11 Poisson Response Modelling
#' #12 Unordered Categorical Responses
#' #13 Ordered Categorical Responses
#' #14 Adjusting for Measurement Errors in Predictor Variables
#' #15 Cross Classified Models
#' #16 Multiple Membership Models
#' #17 Modelling Spatial Data
#' #18 Multivariate Normal Response Models and Missing Data
#' #19 Mixed Response Models and Correlated Residuals
#' #20 Multilevel Factor Analysis Modelling
#' #21 Using Structured MCMC
#' #22 Using the Structured MVN framework for models
#' #23 Using Orthogonal fixed effect vectors
#' #24 Parameter expansion
#' #25 Hierarchical Centring
#' 
#' ## Take Chapter03 as an example
#' ## To find the location of a demo for Chapter03
#' file.show(system.file('demo', 'Chapter03.R', package='R2MLwiN'))
#' 
#' ## To run the demo for Chapter03
#' demo(Chapter03)
#' }
#'
#' @section References:
#' 
#' \subsection{MLwiN software and manuals}{
#' Browne, W.J. (2012) MCMC Estimation in MLwiN, v2.26.
#' Centre for Multilevel Modelling, University of Bristol.
#'
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' MLwiN Version 2.1. Centre for Multilevel Modelling, University of Bristol.
#' 
#' Rasbash, J., Charlton, C. and Pillinger, R. (2012) Manual Supplement to
#' MLwiN v2.26. Centre for Multilevel Modelling, University of Bristol.
#' 
#' Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012)
#' A User's Guide to MLwiN Version 2.26. Centre for Multilevel Modelling,
#' University of Bristol.
#' }
#'
#' \subsection{OpenBUGS}{
#' Thomas, A., O'Hara, B., Ligges, U. and Sturtz, S. (2006) Making BUGS Open.
#' R News, 6, 12:17.
#' }
#'
#' \subsection{WinBUGS}{
#' Spiegelhalter, D.J., Thomas, A. and Best, N.G. (1999) WinBUGS Version 1.2
#' User Manual. MRC Biostatistics Unit.
#' }
#'
#' @section Maintainer:
#' Zhengzheng Zhang \email{zhengzheng236@@gmail.com}
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2015) Centre for Multilevel Modelling, University of Bristol.
#' 
#' @docType package
#' @name R2MLwiN
NULL
 
