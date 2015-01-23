\docType{data}
\name{bes83}
\alias{bes83}
\title{Subsample from British Election Study, '83.}
\format{A data frame with 800 observations on the following 10 variables:
  \describe{
    \item{\code{voter}}{Voter identifier.}
    \item{\code{area}}{Identifier for voters' constituencies.}
    \item{\code{defence}}{Score on a 21 point scale of attitudes towards nuclear weapons with low scores indicating disapproval of Britain possessing them.  This variable is centred about its mean.}
    \item{\code{unemp}}{Score on a 21 point scale of attitudes towards unemployment with low scores indicating strong opposition and higher scores indicating a preference for greater unemployment if it results in lower inflation.  This variable is centred about its mean.}
    \item{\code{taxes}}{Score on a 21 point scale of attitudes towards tax cuts with low scores indicating a preference  for higher taxes to pay for more government spending.  This variable is centred about its mean.}
    \item{\code{privat}}{Score on a 21 point scale of attitudes towards privatization of public services with low scores indicating opposition.  This variable is centred about its mean.}
    \item{\code{votecons}}{If respondent voted Conservative; a factor with levels \code{Other} and \code{Voted_Conservative}.}
    \item{\code{cons}}{This variable is constant (= 1) for all voters.}
    \item{\code{denom}}{This variable is constant (= 1) for all voters.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Heath, A., Yang, M., Goldstein, H. (1996). Multilevel analysis of the changing relationship between class and party in Britain 1964-1992. \emph{Quality and Quantity}, 30:389-404.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) MLwiN Version 2.1. Centre for Multilevel Modelling, University of Bristol.

Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) A User's Guide to MLwiN Version 2.26. Centre for Multilevel Modelling, University of Bristol.
}

\description{
  Subsample from British Election Study, consisting of 800 voters across 110 areas.}
\details{
  The \code{bes83} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009). See Heath et al (1996), and also Rasbash et al (2012) and Browne (2012).
}

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

