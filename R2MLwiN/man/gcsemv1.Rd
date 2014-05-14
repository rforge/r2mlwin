\docType{data}
\name{gcsemv1}
\alias{gcsemv1}
\title{GCSE exam results, taken from 73 schools in England, 1989}
\format{A data frame with 1934 observations on the following 11 variables:
  \describe{
    \item{\code{SCHOOL}}{School identification (level 2 unit).}
    \item{\code{STUDENT}}{Student identification (level 1 unit).}
    \item{\code{FEMALE}}{\code{1} if female, \code{0} if male}
    \item{\code{AGE-MTHS}}{Age in months.}
    \item{\code{WRITTEN}}{Score on the written component.}
    \item{\code{CSEWORK}}{Score on the coursework component.}
    \item{\code{CONS}}{Constant (= 1).}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  GCSE exam results, taken from 73 schools in England, consisting of 1905 pupils across 73 districts.}
\details{
  The \code{bang1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset, with fewer variables.
}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(gcsemv1)

F1 = c(written,csework) ~ (0|cons+female) + (2|cons) + (1|cons)
ID = c('school','student')
(mymodel=runMLwiN(formula=F1, levID=ID, D='Multivariate Normal', indata=gcsecomp1, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

