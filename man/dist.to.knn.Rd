\name{dist.to.knn}
\alias{dist.to.knn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary function for the LOF algorithm. }
\description{
This function returns an object in which columns contain
the indices of the first k neighbors followed by the
distances to each of these neighbors.
}
\usage{
dist.to.knn(dataset, neighbors)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{ The name of the dataset}
  \item{neighbors}{ The number of neighbors }
}
\author{ Caroline Rodriguez}
\seealso{\code{\link{maxlof}}}
\keyword{ math}%
