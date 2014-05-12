\name{caterpillarR}
\alias{caterpillarR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws caterpillar plots of the residuals at a particular level of a multilevel model.
}
\description{
Uses \code{qqmath} in the `lattice' package to draw Quantile-Quantile plots of the residuals at a particular level of a multilevel model against a theoretical distribution.
}
\usage{
## S3 method for class `numeric'
caterpillarR(resi, lev = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resi}{
Either a character string indicating where the residuals are stored or a data.frame object containing the residuals.
}
  \item{lev}{
A digit name indicating which level of a multilevel model residuals are to be plotted for.
}
}

\value{
A caterpillar plot of the residuals at the requested level of the random part of a multilevel model.
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


\seealso{
\code{\link[lattice]{qqmath}}
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
formula=normexam~(0|cons)+(2|cons)+(1|cons)
levID=c('school','student')
estoptions= list(resi.store=TRUE)
mymodel=runMLwiN(formula, levID, indata=tutorial, estoptions=estoptions)

## Caterpillar plot
caterpillarR(mymodel["residual"], lev=2)
}
}

