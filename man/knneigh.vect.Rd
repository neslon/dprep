\name{knneigh.vect}
\alias{knneigh.vect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary function for computing the LOF algorithm}
\description{
Function that returns the distance from a vector "x" to   
its k-nearest-neighbors in the matrix "data"
}
\usage{
knneigh.vect(x, data, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a given instance of the data matrix}
  \item{data}{ the name of the data matrix}
  \item{k}{ the number of neigbors}
}
\author{ Caroline Rodriguez}
\seealso{\code{\link{maxlof}}}
\keyword{math}% at least one, from doc/KEYWORDS
