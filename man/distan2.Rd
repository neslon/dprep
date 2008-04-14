\name{distan2}
\alias{distan2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary function used by the RELIEF function in the dprep library. }
\description{
 Computes the distance between two instances of a dataset considering
both continuous and nominal attributes.
}
\usage{
distan2(x, y, vnom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A given instance  }
  \item{y}{ A given instance}
  \item{vnom}{ A vector containing the indexes of nominal attributes}
}
\author{ Edgar Acuna}
\seealso{\code{\link{relief}}}
\keyword{math}% at least one, from doc/KEYWORDS