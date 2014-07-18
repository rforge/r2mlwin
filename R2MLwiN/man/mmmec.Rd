\docType{data}
\name{mmmec}
\alias{mmmec}
\title{EC data on UV radiation exposure & malignant melanoma.}
\format{A data frame with 354 observations on the following variables:
  \describe{
    \item{\code{nation}}{Nation ID, with levels corresponding to each country.}
    \item{\code{region}}{Region (within-nation) ID.}
    \item{\code{county}}{County (within-region) ID.}
    \item{\code{obs}}{Number of male deaths due to malignant melanoma between 1971 and 1980.}
    \item{\code{exp}}{Expected number of deaths - proportional to county population.}
    \item{\code{cons}}{Constant (=1).}
    \item{\code{uvbi}}{County-level measurement of UV B radiation, centered on the mean.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Langford, I. H., Bentham, G., McDonald, A-L. (1998) Multi-level modelling of geographically aggregated health data: a case study on malignant melanoma mortality and UV exposure in the European Community. \emph{Statistics in Medicine} 17: 41-57.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  EC data on UV radiation exposure & malignant melanoma, consisting of 354 counties across 79 regions across 9 nations.}
\details{
  The \code{mmmec} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013). Further information can be found in Langford et al. (1998) and Browne (2012).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset, with fewer variables.
}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(mmmec)

mmmec[["logexp"]]=double2singlePrecision(log(mmmec[["exp"]]))

F1 = log(obs,logexp) ~ (0|cons+uvbi) + (2|cons) + (3|cons)
ID = c('nation','region','county')
(mymodel3=runMLwiN(formula=F1, levID=ID, D="Poisson", indata=mmmec, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

