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
#' \itemize{
#'  \item{Put}
#'  \item{stuff}
#'  \item{here}
#' }
#'
#' @section Overview:
#' MLwiN uses both classical and Bayesian approaches to fitting multilevel
#' models, and can model continuous, binomial, count, multivariate, ordered
#' categorical and unordered categorical responses. The data structures it can
#' model include hierarchical, cross-classified and/or multiple membership.
#'
#'
#' To use the package, the following objects should be specified:
#' \enumerate{
#' \item a data.frame object containing the data to be modelled;
#' \item a model formula;
#' \item a character (vector) specifying the level ID(s);
#' \item a list of options used to estimate the model;
#' \item a vector specifying the distribution to be modelled;
#' \item a vector specifying BUGS options. If non-null, WinBUGS/OpenBUGS are used, in conjunction
#' with MLwiN, for modelling;
#' \item a path to the folder where the MLwiN executable is saved;
#' \item a path to the folder where the output files are to be saved. By default, the temporary directory
#' (\code{\link{tempdir}}) is used.
#' }
#'
#' Once these objects are specified, the \code{\link{runMLwiN}}
#' function can be used to call MLwiN from R. After execution, the estimates
#' and other statistics are returned to R and can be displayed as a table in
#' the RGui, and the outputs of all parameter estimates (the chains, residuals
#' and BUGs outputs) are also returned for further post-processing in R, as
#' appropriate.
#'
#' @examples
#' \dontrun{
#' library(R2MLwiN)
#' ## Modify the following paths as appropriate.
#' ## MLwiN folder
#' mlwin ="C:/Program Files (x86)/MLwiN v2.29/"
#' ## MLwiN sample worksheet folder
#' wspath=paste(mlwin,"/samples/",sep="")
#' 
#' ## MLwiN sample worksheet: tutorial dataset
#' wsfile=paste(wspath,"tutorial.ws",sep="");inputfile=paste(tempdir(),"/tutorial.dta",sep="")
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
#' file.show(system.file("demo", "Chapter03.R", package="R2MLwiN"))
#' 
#' ## To run the demo for Chapter03
#' demo(Chapter03)
#' }
#'
#' @section References:
#' A User's Guide to MLwiN Version 2.10. Rasbash, J., Steele, F.,
#' Browne, W.J. and Goldstein, H. (2009) Centre for Multilevel Modelling,
#' University of Bristol.
#'
#' @section Maintainer:
#' Zhengzheng Zhang \email{zhengzheng236@@gmail.com}
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2014) Centre for Multilevel Modelling, University of Bristol.
#' 
#' MCMC estimation in MLwiN Version 2.25. Browne, W.J. (2012) Centre for
#' Multilevel Modelling, University of Bristol.
#'
#' @seealso
#' \code{\link{Formula.translate}}, \code{\link{ws2foreign}}, \code{\link[foreign]{read.dta}},
#' \code{\link{runMLwiN}}, \code{\link{MacroScript1}},\ code{\link{MacroScript2}},
#' \code{\link{caterpillar}}, \code{\link{predLines}}, \code{\link{predCurves}}, \code{\link{sixway}}
#'
#' @docType package
#' @name R2MLwiN
NULL

