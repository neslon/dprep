\name{reachability}
\alias{reachability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for computing the reachability function in the LOF algortihm}
\description{
This function computes the reachability function for each instance of a dataset.
This result is used later to compute the Local outlyingness factor.
}
\usage{
reachability(distdata, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distdata}{ the matrix of distances}
  \item{k}{ the given number of neighbors}
}
\author{Caroline Rodriguez}
\seealso{\code{\link{maxlof}}}
\keyword{math}% at least one, from doc/KEYWORDS
