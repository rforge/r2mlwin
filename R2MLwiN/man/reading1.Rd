\docType{data}
\name{reading1}
\alias{reading1}
\title{Students' reading attainment in inner London infant schools.}

\format{A data frame with 407 observations on the following 13 variables:
  \describe{
    \item{\code{id}}{Unique pupil identifying code.}
    \item{\code{age1}}{Age at occasion 1.}
    \item{\code{read1}}{Reading score at occasion 1.}
    \item{\code{age2}}{Age at occasion 2.}
    \item{\code{read2}}{Reading score at occasion 2.}
    \item{\code{age3}}{Age at occasion 3.}
    \item{\code{read3}}{Reading score at occasion 3.}
    \item{\code{age4}}{Age at occasion 4.}
    \item{\code{read4}}{Reading score at occasion 4.}
    \item{\code{age5}}{Age at occasion 5.}
    \item{\code{read5}}{Reading score at occasion 5.}
    \item{\code{age6}}{Age at occasion 6.}
    \item{\code{read6}}{Reading score at occasion 6.}

}}
\source{
Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
Rasbash, J., Steele, F., Browne, W.J., Goldstein, H. (2012) \emph{A User's Guide to MLwiN v2.26}. University of Bristol: Centre for Multilevel Modelling.
Tizard, B., Blatchford, P., Burke, J. & Farquhar, C. (1988). Young children at school in the inner city. Hove, Sussex: Lawrence Erlbaum.
}

\description{
  Reading score data for 407 pupils across 6 occasions.}
\details{
  The \code{reading1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009), and was analysed in Tizard et al. (1988); see also Rasbash et al. (2012) for further details.}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(reading1)




}
}
\keyword{datasets}

