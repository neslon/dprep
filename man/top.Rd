\name{top}
\alias{top}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function for the Bay's Algorithm}
\description{
Function that finds the n candidates for outliers that
were requested by the user.
}
\usage{
top(O, neighbors, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{O}{ n x 1 matrix with the score function from k nearest neighbors }
  \item{neighbors}{ the number of neighbors to be considered }
  \item{n}{ the number of top outliers to search for.}
}
\author{Caroline Rodriguez}
\seealso{\code{\link{baysout}}}
\keyword{methods}% at least one, from doc/KEYWORDS
