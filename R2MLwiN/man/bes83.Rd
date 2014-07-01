\docType{data}
\name{bes83}
\alias{bes83}
\title{Subsample from British Election Study, '83 (see: Heath et al '93; MLwiN manual).}
\format{A data frame with 800 observations on the following 10 variables:
  \describe{
    \item{\code{voter}}{Voter identifier.}
    \item{\code{area}}{Identifier for voters constituencies.}
    \item{\code{defence}}{Score on a 21 point scale of attitudes towards nuclear weapons with low scores indicating disapproval of Britain possessing them.  This variable is centred about its mean.}
    \item{\code{unemp}}{Score on a 21 point scale of attitudes towards unemployment with low scores indicating strong opposition and higher scores indicating a preference for grreater unemployment if it results in lower inflation.  This variable is centred about its mean.}
    \item{\code{taxes}}{Score on a 21 point scale of attitudes towards tax cuts with low scores indicating a preference  for higher taxes to pay for more government spending.  This variable is centred about its mean.}
    \item{\code{privat}}{Score on a 21 point scale of attitudes towards privatization of public services with low scores indicating opposition.  This variable is centred about its mean.}
    \item{\code{votecons}}{= \code{1} if the respondent voted Conservative. = \code{0} otherwise.}
    \item{\code{cons}}{These variables are constant (= 1) for all voters.}
    \item{\code{bcons}}{These variables are constant (= 1) for all voters.}
    \item{\code{denom}}{These variables are constant (= 1) for all voters.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  Subsample from British Election Study, consisting of 800 voters across 110 areas.}
\details{
  The \code{bes83} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), and is a subset of data from the 1989 Bangladesh Fertility Survey (Huq and Cleland, 1990) used by Browne (2012) as an example when fitting logistic models for binary and binomial responses. The full sample was analysed in Amin et al. (1997).}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(bes83)

F1=logit(votecons,cons)~(0|cons+defence+ unemp+ taxes+ privat)+(2|cons)
ID=c('area','voter')
(mymodel=runMLwiN(formula=F1, levID=ID, D="Binomial", indata=bes83, estoptions=list(EstM=1)))

}
}
\keyword{datasets}

