% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.IGLS.R
\name{write.IGLS}
\alias{write.IGLS}
\title{Writes MLwiN macros to fit models using the iterative generalized least
squares (IGLS) algorithm.}
\usage{
write.IGLS(
  indata,
  dtafile,
  oldsyntax = FALSE,
  resp,
  levID,
  expl,
  rp,
  D = "Normal",
  nonlinear = c(0, 1),
  categ = NULL,
  notation = NULL,
  nonfp = NA,
  clre = NULL,
  Meth = 1,
  extra = FALSE,
  reset,
  rcon = NULL,
  fcon = NULL,
  maxiter = 20,
  convtol = 2,
  mem.init = "default",
  optimat = FALSE,
  weighting = NULL,
  fpsandwich = FALSE,
  rpsandwich = FALSE,
  macrofile,
  IGLSfile,
  resifile,
  resi.store = FALSE,
  resioptions,
  debugmode = FALSE,
  startval = NULL,
  namemap = sapply(colnames(indata), as.character),
  saveworksheet = NULL,
  rng.version = 10
)
}
\arguments{
\item{indata}{A data.frame object containing the data to be modelled.}

\item{dtafile}{The name of the temporary file used to send the data to MLwiN, which
is in Stata format (i.e. with extension .dta).}

\item{oldsyntax}{Specified as \code{FALSE} if new syntax has been used in
\code{Formula} object, else specified as \code{TRUE} (to enable
back-compatibility).}

\item{resp}{A character string (vector) of the response variable name(s).}

\item{levID}{A character string (vector) of the specified level ID name(s). The
ID(s) should be sorted in the descending order of levels (e.g.
\code{levID = c('level2', 'level1')} where \code{'level2'} is the higher
level).}

\item{expl}{A character string (vector) of explanatory (predictor)
variable name(s).}

\item{rp}{A character string (vector) of random part of random variable name(s).}

\item{D}{A character string/vector specifying the type of distribution to be modelled, which
can include \code{'Normal'} (the default), \code{'Binomial'}, \code{'Poisson'},
\code{'Negbinom'}, \code{'Unordered Multinomial'}, \code{'Ordered Multinomial'},
\code{'Multivariate Normal'}, or \code{'Mixed'}. In the case of the latter, 
\code{'Mixed'} precedes the response types which also need to be listed in 
\code{D}, e.g. \code{c('Mixed', 'Normal', 'Binomial')}; these need to be
be listed in the same order to which they are referred to in the
\code{Formula} object (see \code{\link{runMLwiN}}, \code{\link{Formula.translate}},
\code{\link{Formula.translate.compat}}. \code{'Mixed'} combinations can consist of
\code{'Normal'} and \code{'Binomial'} or \code{'Normal'} and \code{'Poisson'}.}

\item{nonlinear}{A character vector specifying linearisation method for discrete
response models (see Chapter 9 of Rasbash et al 2012, and Goldstein 2011).
\code{N = 0} specifies marginal quasi-likelihood
linearization (MQL), whilst \code{N = 1} specifies penalised quasi-
likelihood linearization (PQL); \code{M = 1} specifies first order
approximation, whilst \code{M = 2} specifies second order approximation.
\code{nonlinear = c(N = 0, M = 1)} by default. First order marginal
quasi-likelihood (MQL1) only option for single-level discrete response
models.}

\item{categ}{Specifies categorical variable(s) as a matrix. Each column
corresponds to a categorical variable; the first row specifies the name(s)
of variable(s); the second row specifies the name(s) of reference group(s),
\code{NA}(s) if no reference group; the third row states the number of
categories for each variable. Supports back-compatibility with \pkg{R2MLwiN}
versions <0.8-0.}

\item{notation}{Specifies the model subscript notation to be used in the
MLwiN equations window. \code{'class'} means no multiple subscripts, whereas
\code{'level'} has multiple subscripts.}

\item{nonfp}{Removes the fixed part of random variable(s). \code{NA} if no
variable to be removed.}

\item{clre}{A matrix used to define which elements of the random effects matrix
to remove (i.e. hold constant at zero). Removes
from the random part at level <first row> the covariance matrix element(s)
defined by the pair(s) of rows <second row> <third row>. Each column
corresponds to a removed entry of the covariance matrix. See e.g. \code{demo(UserGuide07)}
for an example.}

\item{Meth}{Specifies which maximum likelihood estimation method to be used.
If \code{Meth = 0} estimation method is set to RIGLS. If \code{Meth = 1}
estimation method is set to IGLS (the default setting).}

\item{extra}{If \code{TRUE}, extra binomial, extra negative binomial,
extra Poisson or extra multinomial distributions assumed, else \code{FALSE}.}

\item{reset}{A vector of \code{length(levID)} specifying the action to be
taken, at each level, if a variance parameter is estimated at a particular
iteration to be negative during estimation. Values specified in
ascending order of level hierarchy: if \code{0} a negative variance
estimate is reset to zero and so are any associated covariances; if \code{1}
a negative variance estimate is reset to zero but not the associated
covariances; if \code{2} no resetting takes place. E.g. \code{reset = c(0, 1)}
to assign value \code{0} to level 1 and value \code{1} to level 2 of
two-level model.}

\item{rcon}{Matrix specifying constraints on the random parameters as
specified in \code{random.ui} and \code{random.ci} in the \code{constraints}
option within \code{estoptions} (see \code{\link{runMLwiN}}). \code{random.ci}
is appended to the bottom row of \code{random.ui}.}

\item{fcon}{Matrix specifying constraints on the fixed coefficients as
specified in \code{fixed.ui} and \code{fixed.ci} in the \code{constraints}
option within \code{estoptions} (see \code{\link{runMLwiN}}). \code{fixed.ci}
is appended to the bottom row of \code{fixed.ui}.}

\item{maxiter}{Numeric value specifying the maximum number of iterations, from
the start, before estimation halts.}

\item{convtol}{Numeric value specifying the convergence criterion, as
specified in the \code{tol} option within \code{estoptions} (see
\code{\link{runMLwiN}}). If value of \code{convtol} is m, estimation will be
deemed to have converged when the relative change in the estimate for all
parameters from one iteration to the next is less than 10(-m). Defaults to
value of \code{2} for m if not otherwise specified.}

\item{mem.init}{If calling write.IGLS directly, if wish to use defaults, value needs to be
specified as \code{'default'}, else specify a vector of length 5 corresponding
to the following order: number of levels; worksheet size in thousands of cells;
the number of columns; the number of explanatory variables; the number of group
labels.}

\item{optimat}{This option instructs MLwiN to limit the maximum matrix size
that can be allocated by the (R)IGLS algorithm. Specify \code{optimat = TRUE}
if MLwiN gives the following error message 'Overflow allocating smatrix'.
This error message arises if one more higher-level units is extremely large
(contains more than 800 lower-level units). In this situation \pkg{R2MLwiN}'s
default behaviour is to instruct MLwiN to allocate a larger matrix size to
the (R)IGLS algorithm than is currently possible. Specifying
\code{optimat = TRUE} caps the maximum matrix size at 800 lower-level units,
circumventing the MLwiN error message, and allowing most MLwiN
functionality.}

\item{weighting}{A list of two items, one of which is a list called \code{weightvar}
the length of which corresponds
to the number of levels in the model, in descending order from highest level first.
The other is an option \code{standardised} which is \code{TRUE} or \code{FALSE}.}

\item{fpsandwich}{Specifies standard error type for fixed parameters. If
\code{fpsandwich = TRUE}, robust or `sandwich' standard errors based on raw
residuals are used, if \code{fpsandwich = FALSE} (default) then standard,
uncorrected, IGLS or RIGLS computation used.}

\item{rpsandwich}{Specifies standard error type for random parameters. If
\code{rpsandwich = TRUE}, robust or `sandwich' standard errors based on raw
residuals are used, if \code{rpsandwich = FALSE} (default) then standard,
uncorrected, IGLS or RIGLS `plug in' estimates used.}

\item{macrofile}{A file name where the MLwiN macro file will be saved. The
default location is in the temporary folder.}

\item{IGLSfile}{A file name where the parameter estimates will be saved. The
default location is in the temporary folder.}

\item{resifile}{A file name where the residuals will be saved. The default
location is in the temporary folder.}

\item{resi.store}{A logical value to indicate if the residuals are to be
stored (\code{TRUE}) or not (\code{FALSE}).}

\item{resioptions}{A string vector to specify the various residual options.
The \code{'variances'} option calculates the posterior variances instead of
the posterior standard errors; the \code{'standardised'}, \code{'leverage'},
\code{'influence'} and \code{'deletion'} options calculate standardised,
leverage, influence and deletion residuals respectively; the
\code{'sampling'} option calculates the sampling variance covariance matrix
for the residuals; the \code{'norecode'} option prevents residuals with
values exceedingly close or equal to zero from being recoded to missing; the
reflate option returns unshrunken residuals. Note that the default option is
\code{resioptions = c('variance')}; \code{'variance'} cannot be used together
with the other options to calculate standardised, leverage, influence and
deletion residuals.}

\item{debugmode}{A logical value determining whether MLwiN is run in the
background or not. The default value is \code{FALSE}: i.e. MLwiN is run in
the background. If \code{TRUE} the MLwiN GUI is opened, and then pauses after the model
has been set-up, allowing user to check starting values; pressing 'Resume macro'
will then fit the model. Once fit, pressing 'Resume macro' once more will save
the outputs to the \code{workdir} ready to be read by \pkg{R2MLwiN}. Users can
instead opt to 'Abort macro' in which case the outputs are not saved to the
\code{workdir}. This option currently
works for 32 bit version of MLwiN only (automatically switches unless
\code{MLwiNPath} or \code{options(MLwiNPath)}
has been set directly to the executable).}

\item{startval}{A list of numeric vectors specifying the starting values.
\code{FP.b} corresponds to the estimates for the fixed
part; \code{FP.v} specifies the variance/covariance estimates for the fixed
part; \code{RP.b} specifies the variance estimates for the random part;
\code{RP.v} corresponds to the variance/covariance matrix of the variance
estimates for the random part.}

\item{namemap}{A mapping of column names to DTA friendly shorter names}

\item{saveworksheet}{A file name used to store the MLwiN worksheet after the
model has been estimated.}

\item{rng.version}{An integer indicating which random number generator should
be used by MLwiN.}
}
\value{
Outputs a modified version of namemap containing newly generated
short names.
}
\description{
write.IGLS is an internal function which creates an MLwiN macro file to
fit a multilevel model using IGLS.
}
\references{
Goldstein, H. (2011) Multilevel Statistical Models. 4th Edition. London: John Wiley and Sons.

Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012)
A User's Guide to MLwiN Version 2.26. Centre for Multilevel Modelling,
University of Bristol.
}
\seealso{
\code{\link{write.MCMC}}
}
\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
}
