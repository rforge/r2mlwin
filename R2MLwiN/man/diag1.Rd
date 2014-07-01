\docType{data}
\name{diag1}
\alias{diag1}
\title{Diag dataset}
\format{A data frame with 907 observations on the following 9 variables:
  \describe{
    \item{\code{school}}{School identifier.}
    \item{\code{sex}}{Pupil gender.}
    \item{\code{vrq}}{School type (0=comprehansive, 1=grammar).}
    \item{\code{ilea}}{O-level/CSE examination results.}
    \item{\code{type}}{Verbal Reasoning quotient.}
    \item{\code{pupil}}{Pupil identifier.}
    \item{\code{cons}}{Constant (=1).}
    \item{\code{n_ilea}}{O-level/CSE examination results (normal scores).}
    \item{\code{n_vrq}}{Verbal Reasoning quotient (normal scores).}
}}
\source{
Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
Aitkin, M. & Longford, N. (1986). Statistical modelling in school effectiveness studies (with discussion). Journal of the Royal Statistical Society, Series A, 149:1–43.
}

\description{
  Examination data for, consisting of 907 students within 18 schools.}
\details{
  The \code{diag1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), and was analysed in Langford and Lewis (1998).}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(diag1)


}
}
\keyword{datasets}

