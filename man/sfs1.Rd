\name{sfs1}
\alias{sfs1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One-step sequential forward selection}
\description{
This function computes one-step of the sequential forward selection
procedure.}
\usage{
sfs1(data, indic, correcto, kvec, method = c("lda", "knn",
 "rpart"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ Name of the dataset to be used.}
  \item{indic}{ A vector of 0-1 values. }
  \item{correcto}{ The recognition rate in the previuos step.}
  \item{kvec}{ The number of neighbors to be used by the knn classifier}
  \item{method}{ The classifier to be used to select the best features.}
}
\author{Edgar Acuna}
\seealso{\code{\link{sffs}}}
\keyword{methods}
