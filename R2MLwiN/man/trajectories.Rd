\name{trajectories}
\alias{trajectories}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots MCMC chain trajectories
}
\description{
This function draws trajectories of MCMC chains.
}
\usage{
trajectories(chains, Range = c(1, 5000), selected = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chains}{
A data frame of parameter chains from MLwiN.
}
  \item{Range}{
An integer vector of length two specifying the first and last iterations of the chains.
}
  \item{selected}{
A character vector specifying the selected chains to be plotted.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\code{\link{sixway}}
}
\examples{
\dontrun{
library(R2MLwiN)
#NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#...so please change relevant line if different
#if using R2MLwiN via WINE, the path may look like 
#options(MLwiN_path="/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/") 

## Example: tutorial
data(tutorial)
formula=normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)
levID=c('school','student')
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, indata=tutorial, estoptions=estoptions)

trajectories(mymodel["chains"],Range=c(4501,5000))
}
}
