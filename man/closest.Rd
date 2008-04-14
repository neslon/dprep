\name{closest}
\alias{closest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function used in the function baysout}
\description{
 Function used by baysout to select the k vectors that are closer to 
 a given instance.  
}
\usage{
closest(dis, neigh, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dis}{  An instance from the dataset under study}
  \item{neigh}{  A matrix containing the distance from the given observations 
              to each of its k neighbors.}
  \item{k}{ The number of nearest neighbors}
}
\author{Caroline Rodriguez}
\seealso{\code{\link{baysout}}}
\keyword{math}
