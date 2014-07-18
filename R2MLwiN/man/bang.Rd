\docType{data}
\name{bang}
\alias{bang}
\title{Sub-sample from the 1989 Bangladesh Fertility Survey (see Huq & Cleveland, 1990)}
\format{A data frame with 2867 observations on the following 12 variables:
  \describe{
    \item{\code{woman}}{Identifying code for each woman (level 1 unit).}
    \item{\code{district}}{Identifying code for each district (level 2 unit).}
    \item{\code{use}}{Contraceptive use status at time of survey; levels are \code{Not using} and \code{Using}.}
    \item{\code{use4}}{Contraceptive use status and method (with levels: \code{1} = Sterilization, \code{2} = Modern reversible method, \code{3} = Traditional method, \code{4} = Not using contraception)}
    \item{\code{lc}}{Number of living children at time of survey; ordered levels are \code{None}, \code{One child}, \code{Two children}, \code{Three plus}.}
    \item{\code{age}}{Age of woman at time of survey (in years), centred on sample mean of 30 years.}
    \item{\code{urban}}{Type of region of residence; levels are \code{Rural} and \code{Urban}.}
    \item{\code{educ}}{Woman's level of education (ordered levels are \code{None}, \code{Lower primary}, \code{Upper primary}, \code{Secondary and above}.}
    \item{\code{hindu}}{Woman's religion; levels are \code{Muslim} and \code{Hindu}.}
    \item{\code{d_lit}}{Proportion of women in district who are literate.}
    \item{\code{d_pray}}{Proportion of Muslim women in district who pray every day (a measure of religiosity).}    
    \item{\code{cons}}{Constant of ones.}
}}
\source{
Amin, S., Diamond, I., Steele, F. (1997) Contraception and religiosity in Bangladesh. In: G. W. Jones, J. C. Caldwell, R. M. Douglas, R. M. D'Souza (eds) \emph{The Continuing Demographic Transition}, 268--289. Oxford: Oxford University Press.

Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Huq, N. M., Cleland, J. (1990) \emph{Bangladesh fertility survey, 1989.} Dhaka: National Institute of Population Research and Training (NIPORT).

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  A subset of data from the 1989 Bangladesh Fertility Survey, consisting of 2867 women across 61 districts.}
\details{
  The \code{bang} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), and is a subset of data from the 1989 Bangladesh Fertility Survey (Huq and Cleland, 1990) used by Browne (2012) as an example when fitting logistic models for binary and binomial responses. The full sample was analysed in Amin et al. (1997).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset, with fewer variables.
}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(bang)

F1=log(use4,cons,use4_4)~(0|cons+lc[lc0])+(2|cons)
ID=c('district','woman')
estoptions= list(EstM=1, nonlinear=c(1,2))

(mymodel=runMLwiN(formula=F1, levID=ID, D='Unordered Multinomial', 
                  indata=bang, estoptions=estoptions))

}
}
\keyword{datasets}

