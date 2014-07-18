\docType{data}
\name{diag1}
\alias{diag1}
\title{Examination dataset}
\format{A data frame with 907 observations on the following 9 variables:
  \describe{
    \item{\code{school}}{School identifier.}
    \item{\code{sex}}{Pupil gender.}
    \item{\code{vrq}}{Verbal Reasoning quotient.}
    \item{\code{ilea}}{O-level/CSE examination results.}
    \item{\code{type}}{School type (with levels \code{Comprehensive} and \code{Grammar}).}
    \item{\code{pupil}}{Pupil identifier.}
    \item{\code{cons}}{Constant (=1).}
    \item{\code{n_ilea}}{O-level/CSE examination results (normal scores).}
    \item{\code{n_vrq}}{Verbal Reasoning quotient (normal scores).}
}}
\source{
Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
Rasbash, J., Steele, F., Browne, W.J., Goldstein, H. (2012) \emph{A User's Guide to MLwiN v2.26}. University of Bristol: Centre for Multilevel Modelling.
Aitkin, M. & Longford, N. (1986). Statistical modelling in school effectiveness studies (with discussion). Journal of the Royal Statistical Society, Series A, 149:1–43.
}

\description{
  Examination data for 907 students within 18 schools.}
\details{
  The \code{diag1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), originally analysed in Aitkin & Longford (1986), and described further in Rasbash et al. (2012).}

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

