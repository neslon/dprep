\name{distancia1}
\alias{distancia1}
\title{Vector-Vector Manhattan Distance Function}
\description{
  Finds the Manhattan distance between two vectors x and y, or 
the vector x and the matrix y}
\usage{
distancia1(x, y)
}
\arguments{
  \item{x}{ A numeric vector}
  \item{y}{ A numeric vector or matrix}
}
\details{
  Does not support missing values.
}
\value{
  \item{distancia}{ The result is a numeric value representing the Manhattan distance between
x and y, or a row matrix representing the Euclidean distance between x and
each row of y.
}
}
\author{Edgar Acuna }
\examples{
#---- Calculating distances
x=rnorm(4)
y=matrix(rnorm(12),4,3)
distancia1(x,y[,1])
distancia1(x,y)
}

\keyword{math}
