\name{nnmiss}
\alias{nnmiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary function for knn imputation}
\description{
 This function is required to perform k-nn imputation.
}
\usage{
nnmiss(x, xmiss, ismiss, xnom, K = 1)
}
\arguments{
  \item{x}{ A submatrix of complete rows from original matrix}
  \item{xmiss}{A a row with a missing value}
  \item{ismiss}{A vector that indicates whether a value in xmiss is missing or not}
  \item{xnom}{ A vector with indexes of nominal variables }
  \item{K}{ The number of neighbors to use }
}
\author{ Edgar Acuna}
\seealso{\code{\link{ce.impute}}}
\keyword{manip}
