\docType{data}
\name{rats}
\alias{rats}
\title{Weights of 30 rats, measured weekly over 5 weeks.}
\format{A data frame with 30 observations on the following 7 variables:
  \describe{
    \item{\code{y8}}{Weight on day 8.}
    \item{\code{y15}}{Weight on day 15.}
    \item{\code{y22}}{Weight on day 22.}
    \item{\code{y29}}{Weight on day 29.}
    \item{\code{y36}}{Weight on day 36.}
    \item{\code{cons}}{Constant(=1).}
    \item{\code{rat}}{Rat ID}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Gelfand, Alan E., et al. "Illustration of Bayesian inference in normal data models using Gibbs sampling." Journal of the American Statistical Association 85.412 (1990): 972-985.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  Weights of 30 rats, measured weekly over 5 weeks.}
\details{
  The \code{rats} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013).}

}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(rats)

F1 = c(y8,y15,y22,y29,y36) ~ (0|cons) + (1|cons)
ID = c('rat')
(mymodel=runMLwiN(formula=F1, levID=ID, D='Multivariate Normal', indata=rats, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

