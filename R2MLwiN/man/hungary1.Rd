\docType{data}
\name{hungary1}
\alias{hungary1}
\title{Hungarian component of 2nd International Science Survey, '84; see Goldstein 2003}
\format{A data frame with 2439 observations on the following 10 variables:
  \describe{
    \item{\code{woman}}{Identifying code for each woman (level 1 unit).}
    \item{\code{district}}{Identifying code for each district (level 2 unit).}
    \item{\code{use}}{Contraceptive use status at time of survey; levels are \code{Not using} and \code{Using}.}
    \item{\code{lc}}{Number of living children at time of survey; levels correspond to none (\code{nokids}), one (\code{onekid}), two (\code{twokids}), or three or more children (\code{threeplus}).}
    \item{\code{age}}{Age of woman at time of survey (in years), centred on sample mean of 30 years.}
    \item{\code{urban}}{Type of region of residence; levels are \code{Rural} and \code{Urban}.}
    \item{\code{educ}}{Woman's level of education (\code{1} = None, \code{2} = Lower primary, \code{3} = Upper primary, \code{4} = Secondary and above.}
    \item{\code{hindu}}{Woman's religion; levels are \code{Muslim} and \code{Hindu}.}
    \item{\code{d_illit}}{Proportion of women in district who are literate.}
    \item{\code{d_pray}}{Proportion of Muslim women in district who pray every day (a measure of religiosity).}    
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Goldstein, H.. 2003. Multilevel Statistical Models. Third Edition. London, Edward Arnold.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  Hungarian component of 2nd International Science Survey, consisting of 2439 women across 99 districts.}
\details{
  The \code{hungary1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013).}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(hungary1)

F1 = c(es_core,biol_core,biol_r3,biol_r4,phys_core,phys_r2) ~ (0|cons+female) + (2|cons) + (1|cons)
ID = c('school','student')
(mymodel=runMLwiN(formula=F1, levID=ID, D='Multivariate Normal', indata=hungary1, estoptions=list(EstM=1)))
}
}
\keyword{datasets}

