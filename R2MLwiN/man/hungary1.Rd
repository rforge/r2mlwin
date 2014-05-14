\docType{data}
\name{hungary1}
\alias{hungary1}
\title{Hungarian component of 2nd International Science Survey, '84; see Goldstein 2003}
\format{A data frame with 2439 observations on the following 10 variables:
  \describe{
    \item{\code{school}}{Identifying code for each school (level 2 unit).}
    \item{\code{female}}{Gender indicator (\code{0} = Male, \code{1} = Female).}
    \item{\code{es_core}}{Core Earth Scienses test result.}
    \item{\code{biol_core}}{Core Biology test result.}
    \item{\code{biol_r3}}{Optional Biology test result.}
    \item{\code{biol_r4}}{Optional Biology test result.}
    \item{\code{phys_core}}{Core Physics test result.}
    \item{\code{phys_r2}}{Optional Physics test result.}
    \item{\code{cons}}{Constant(=1).}
    \item{\code{student}}{Identifying code for each student (level 1 unit).}    
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Goldstein, H.. 2003. Multilevel Statistical Models. Third Edition. London, Edward Arnold.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  Hungarian component of 2nd International Science Survey, consisting of 2439 women across 99 districts.}
\details{
  The \code{hungary1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013).}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(hungary1)

F1 = c(es_core,biol_core,biol_r3,biol_r4,phys_core,phys_r2) ~ (0|cons+female) + (2|cons) + (1|cons)
ID = c('school','student')
(mymodel=runMLwiN(formula=F1, levID=ID, D='Multivariate Normal', indata=hungary1, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

