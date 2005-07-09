\name{nnmiss}
\alias{nnmiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary function for knn imputation}
\description{
 This function is required to perform k-nn imputation
}
\usage{
nnmiss(x, xmiss, ismiss, xnom, K = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{submatrix of complete rows from original matrix}
  \item{xmiss}{a row with a missing value}
  \item{ismiss}{vector that indicates whether a value in xmiss is missing or not}
  \item{xnom}{ vector with indexes of nominal variables }
  \item{K}{ the number of neighbors }
}
}
\author{ Caroline Rodriguez}
\seealso{\code{\link{ce.impute}}}
\keyword{manip}% at least one, from doc/KEYWORDS
