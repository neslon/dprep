\name{sbs1}
\alias{sbs1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One-step sequential backward selection}
\description{
This functions performs one-step of the sequential backward selection
procedure.}
\usage{
sbs1(data, indic, correct0, kvec, method = c("lda", "knn", "rpart"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The name of a dataset}
  \item{indic}{ A vector of 0-1 values: 1 indicates a selected feature.}
  \item{correct0}{ The recognition rate based on the current subset of features}
  \item{kvec}{ The number of neighbors}
  \item{method}{ The classifier to be used}
}
\author{ Edgar Acuna}
\seealso{\code{\link{sffs}}}
\keyword{Feature Selection}
