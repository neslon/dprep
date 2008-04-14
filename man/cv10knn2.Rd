\name{cv10knn2}
\alias{cv10knn2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function for sequential feature selection}
\description{
  This function finds the number of instances correctly classified 
by the knn classifier, using 10-fold cross validation, with one 
repetition
}
\usage{
cv10knn2(data, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The name of the dataset}
  \item{kvec}{ The number of neighbors}
}
\author{Edgar Acuna}
\seealso{\code{\link{crossval}}}
\keyword{classif}
