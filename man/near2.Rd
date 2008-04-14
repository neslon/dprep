\name{near2}
\alias{near2}
\title{Auxiliary function for the reliefcat function}
\description{
This function finds the instance in the data matrix 
that is closest to a given instance x. The
attributes can be either continuous or nominal.
}
\usage{
near2(x, data, vnom)
}
\arguments{
  \item{x}{ A given instance}
  \item{data}{ The name of the dataset}
  \item{vnom}{ A vector of indexes for nominal attributes}
}
\author{Edgar Acuna}
\seealso{ \code{\link{relief}},\code{\link{near1}}}

\keyword{math}
