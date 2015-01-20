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
#' A new model-fitting syntax, more in keeping with that conventionally used in
#' R, was introduced in R2MLwiN version 0.8-0, with back-compatibility
#' with earlier syntax preserved.
#' \itemize{
#'  \item{Put}
#'  \item{stuff}
#'  \item{here}
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
 
