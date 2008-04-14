\name{maxdist}
\alias{maxdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function used when executing the Bay's algorithm for outlier detection }
\description{
This function is used by the function baysout in this package, to find the largest value of a 
distance vector. Returns the value and the index number of the largest distance.
}
\usage{
maxdist(dneighbors)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dneighbors}{ The value and the index number of the largest distance}
}
\author{Caroline Rodriguez}
\seealso{\code{\link{baysout}}}

\keyword{ math }
