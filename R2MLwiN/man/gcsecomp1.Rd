\docType{data}
\name{gcsecomp1}
\alias{gcsecomp1}
\title{Pupils' marks from GCSE exams (UK, 1989); complete cases only.}
\format{A data frame with 1523 observations on the following 6 variables:
  \describe{
    \item{\code{school}}{Identifying code for each school (level 2 unit).}
    \item{\code{student}}{Identifying code for each pupil (level 1 unit).}
    \item{\code{female}}{Gender of pupil: a factor with levels \code{Male} and \code{Female}.}
    \item{\code{written}}{Exam score.}
    \item{\code{csework}}{Coursework score.}
    \item{\code{cons}}{Constant (=1).}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) MLwiN Version 2.1. Centre for Multilevel Modelling, University of Bristol.
}

\description{
  Pupils' marks from GCSE exams, consisting of 1523 pupils across 73 schools. See Browne (2012) for further details.}
\details{
  The \code{gcsecomp1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009).}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(gcsecomp1)

F1 = c(written,csework) ~ (0|cons+female) + (2|cons) + (1|cons)
ID = c('school','student')
(mymodel=runMLwiN(formula=F1, levID=ID, D='Multivariate Normal', 
                  indata=gcsecomp1, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

