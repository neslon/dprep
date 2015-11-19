\name{reachability}
\alias{reachability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for computing the reachability measure in the LOF algortihm}
\description{
This function computes the reachability measure for each instance of a dataset.
This result is used later to compute the Local Outlyingness Factor.
}
\usage{
reachability(distdata, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distdata}{ The matrix of distances}
  \item{k}{ The given number of neighbors}
}
\author{Caroline Rodriguez}
\seealso{\code{\link{maxlof}}}

\keyword{math}% at least one, from doc/KEYWORDS

