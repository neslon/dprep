\name{distancia}
\alias{distancia}
\title{Vector-Vector Euclidiean Distance Function}
\description{
  Finds the euclidean distance between two vectors x and y, or 
the vector x and the matrix y}
\usage{
distancia(x, y)
}
\arguments{
  \item{x}{ A numeric vector}
  \item{y}{ A numeric vector or matrix}
}
\details{
  Does not support missing values.
}
\value{
  \item{distancia}{ The result is a numeric value representing the Euclidean distance between
x and y, or a row matrix representing the Euclidean distance between x and
each row of y.
}
}
\author{Caroline Rodriguez and Edgar Acuna }
\examples{
#---- Calculating distances
x=rnorm(4)
y=matrix(rnorm(12),4,3)
distancia(x,y[,1])
distancia(x,y)
}

\keyword{math}
