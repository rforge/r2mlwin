\docType{data}
\name{gcsemv1}
\alias{gcsemv1}
\title{Pupils' marks from GCSE exams (UK, 1989); see MLwiN MCMC manual (Browne, 2012).}
\format{A data frame with 1905 observations on the following variables:
  \describe{
    \item{\code{school}}{School identification (level 2 unit).}
    \item{\code{student}}{Student identification (level 1 unit).}
    \item{\code{female}}{Gender, with levels \code{Female} and \code{Male}.}
    \item{\code{agemths}}{Age in months.}
    \item{\code{written}}{Score on the written component.}
    \item{\code{csework}}{Score on the coursework component.}
    \item{\code{cons}}{Constant (= 1).}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  GCSE exam results, taken from 73 schools in England, consisting of 1905 pupils.}
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
(mymodel=runMLwiN(formula=F1, levID=ID, D='Multivariate Normal',
                  indata=gcsecomp1, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

