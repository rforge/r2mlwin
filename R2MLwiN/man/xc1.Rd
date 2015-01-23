\docType{data}
\name{xc1}
\alias{xc1}
\title{Examination scores of 16-year olds in Fife, Scotland.}
\format{A data frame with 3435 observations on the following 11 variables:
  \describe{
    \item{\code{vrq}}{A verbal reasoning score resulting from tests pupils took when they entered secondary school.}
    \item{\code{attain}}{Attainment score of pupils at age sixteen.}
    \item{\code{pid}}{Primary school identifying code.}
    \item{\code{sex}}{Pupils' gender: a factor with levels \code{Male} and \code{Female}.}
    \item{\code{sc}}{Pupils' social class (scaled from low to high).}
    \item{\code{sid}}{Secondary school identifying code.}
    \item{\code{fed}}{Fathers' education.}
    \item{\code{choice}}{Choice number of secondary school attended (where 1 is first choice, etc.)}
    \item{\code{med}}{Mothers' education.}
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
    \item{\code{pupil}}{Pupil identifying code.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Paterson, L. (1991) Socio economic status and educational attainment: a multidimensional and multilevel study. Evaluation and Research in Education, 5, 97-121.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) MLwiN Version 2.1. Centre for Multilevel Modelling, University of Bristol.
}

\description{
  A dataset of examination scores of 16-year olds in Fife, Scotland, in which the secondary school the pupil attended is cross-classified by the primary school the pupil attended.}
\details{
  The \code{xc1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009), analysed by Paterson (1991). The data are cross-classified in that not all children who attended the same primary school subsequently entered the same secondary school. See also Browne (2012).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset.
}

\examples{
\dontrun{
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/")

data(xc1)
# Fit cross-classified model, partitioning variance in ATTAIN across
# and within primary (pid) and secondary (sid) school
F1 = attain ~ (0|cons) + (3|cons) + (2|cons) + (1|cons)
ID = c("sid", "pid", "pupil")
MyXC <- list("class" = c(3, 2),"N1" = c(1, 1))
(XCModel <- runMLwiN(Formula = F1, levID = ID, indata = xc1, 
estoptions = list(xclass = MyXC, EstM = 1)))
}
}
\keyword{datasets}

