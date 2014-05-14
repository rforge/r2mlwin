\docType{data}
\name{lips1}
\alias{lips1}
\title{Lips data}
\format{A data frame with 56 observations on the following 41 variables:
  \describe{
    \item{\code{area}}{Region ID.}
    \item{\code{cons}}{Constant (=1).}
    \item{\code{obs}}{Observed cases of lip cancer.}
    \item{\code{exp}}{Expected count.}
    \item{\code{perc_aff}}{Percentage of the region who work in agriculture, fishing and forestry.}
    \item{\code{offs}}{Log of the expected count.}
    \item{\code{pcons}}{Constant (=1).}
    \item{\code{denom}}{Constant (=1).}
    \item{\code{neigh1}}{First neighbours.}
    \item{\code{neigh2}}{Second neighbours.}
    \item{\code{neigh3}}{Third neighbours.}
    \item{\code{neigh4}}{Fourth neighbours.}
    \item{\code{neigh5}}{Fifth neighbours.}
    \item{\code{neigh6}}{Sixth neighbours.}
    \item{\code{neigh7}}{Seventh neighbours.}
    \item{\code{neigh8}}{Eighth neighbours.}
    \item{\code{neigh9}}{Nineth neighbours.}
    \item{\code{neigh10}}{Tenth neightbours.}
    \item{\code{neigh11}}{Eleventh neightbours.}
    \item{\code{weight1}}{First neighbours' weights.}
    \item{\code{weight2}}{Second neighbours' weights.}
    \item{\code{weight3}}{Third neighbours' weights.}
    \item{\code{weight4}}{Fourth neighbours' weights.}
    \item{\code{weight5}}{Fifth neighbours' weights.}
    \item{\code{weight6}}{Sixth neighbours' weights.}
    \item{\code{weight7}}{Seventh neighbours' weights.}
    \item{\code{weight8}}{Eighth neighbours' weights.}
    \item{\code{weight9}}{Nineth neighbours' weights.}
    \item{\code{weight10}}{Tenth neightbours' weights.}
    \item{\code{weight11}}{Eleventh neightbours' weights.}
    \item{\code{wcar1}}{First neighbours' CAR weights.}
    \item{\code{wcar2}}{Second neighbours' CAR weights.}
    \item{\code{wcar3}}{Third neighbours' CAR weights.}
    \item{\code{wcar4}}{Fourth neighbours' CAR weights.}
    \item{\code{wcar5}}{Fifth neighbours' CAR weights.}
    \item{\code{wcar6}}{Sixth neighbours' CAR weights.}
    \item{\code{wcar7}}{Seventh neighbours' CAR weights.}
    \item{\code{wcar8}}{Eighth neighbours' CAR weights.}
    \item{\code{wcar9}}{Nineth neighbours' CAR weights.}
    \item{\code{wcar10}}{Tenth neightbours' CAR weights.}
    \item{\code{wcar11}}{Eleventh neightbours' CAR weights.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Clayton, David, and John Kaldor. "Empirical Bayes estimates of age-standardized relative risks for use in disease mapping." Biometrics 43.3 (1987): 671-681.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  Lips data, consisting of 56 observations across 56 districts.}
\details{
  The \code{lips1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013).}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(lips1)

F1 = log(obs,offs) ~ (0|cons+perc_aff) + (2|cons)
ID = c('neigh1','area','area')
(mymodel=runMLwiN(formula=F1, levID=ID, D='Poisson', indata=lips1, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

