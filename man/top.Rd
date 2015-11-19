\name{top}
\alias{top}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function for Bay's Ouylier Detection Algorithm}
\description{
Function that finds the number of candidate outliers requested by the user.
}
\usage{
top(O, neighbors, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{O}{ An n x 1 matrix with the score function from k nearest neighbors }
  \item{neighbors}{ The number of neighbors to be considered }
  \item{n}{ The number of top outliers to search for.}
}
\author{Caroline Rodriguez}
\seealso{\code{\link{baysout}}}

\keyword{math}
