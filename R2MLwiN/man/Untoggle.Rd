\name{Untoggle}
\alias{Untoggle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a categorical variable into several separate binary variables
}
\description{
This function converts a vector (factor) of categorical character strings (integers) into several separate vectors of binary indicators.
}
\usage{
Untoggle(categrv, name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categrv}{
A vector (factor) of categorical character strings (integers).
}
  \item{name}{
A character string specifying a name of the categorical variable. Note that this option is only used when the categorical labels are integers.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
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
names(tutorial)
tutorial=cbind(tutorial,Untoggle(tutorial[["school"]],"school"))
names(tutorial)
}
}

