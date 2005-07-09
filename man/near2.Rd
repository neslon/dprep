\name{near2}
\alias{near2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function for the reliefcat function}
\description{
This function finds out the instance in the data matrix 
that is closest to a given instance x. The
attributes could be either continuous or nominal.
}
\usage{
near2(x, data, vnom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a given instance}
  \item{data}{ the name of the dataset}
  \item{vnom}{ a vector of indexes for nominal attributes}
}
\author{Edgar Acuna}
\seealso{ \code{\link{relief}},\code{\link{near1}}}
\keyword{math}% at least one, from doc/KEYWORDS
