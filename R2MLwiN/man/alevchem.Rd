\docType{data}
\name{alevchem}
\alias{alevchem}
\title{Chemistry A-level results from one exam board}
\format{A data frame with 2166 observations on the following 8 variables:
  \describe{
    \item{\code{lea}}{Local Education Authority ID.}
    \item{\code{estab}}{Establishment (institution) ID.}
    \item{\code{pupil}}{Pupil ID.}
    \item{\code{a_point}}{A-level point score (with levels: \code{F}, \code{E}, \code{D}, \code{C}, \code{B}, \code{A}).}
    \item{\code{gcse_tot}}{Total GCSE point score.}
    \item{\code{gcse_no}}{Number of GCSEs taken.}
    \item{\code{cons}}{Constant of ones}
    \item{\code{gender}}{Pupil's gender (with levels: \code{male}, \code{female}).}
}}

\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.

Yang, M., Woodhouse, G. (2001) Progress from GCSE to A and AS level: institutional and gender differences, and trends over time. \emph{British Educational Research Journal} 27: 245-267.
}

\description{
  Chemistry A-level results from one exam board; subset from Yang & Woodhouse, 2001.}
\details{
  The \code{alevchem} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013).}


\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(alavchem)

alevchem["gcse2"]=double2singlePrecision(alevchem["gcseav"]^2)

# Fit multilevel multinomial model
F1=logit(a_point,cons,A) ~ (`0s`|cons) + (`0c`|gcseav+gcse2+gender) + (`2c` | cons+gcseav)

ID=c('estab','pupil')

## Fit the model

(mymodel <- runMLwiN(formula=F1, levID=ID, D='Ordered Multinomial', 
                     indata=alevchem, estoptions=list(EstM=1)))

}
}
\keyword{datasets}

