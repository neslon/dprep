\name{distancia}
\alias{distancia}
\title{Vector-Vector Euclidiean Distance Function}
\description{
  Finds the euclidean distance between two vectors x and y, or 
the matrix y and the vector x
}
\usage{
distancia(x, y)
}
\arguments{
  \item{x}{ numeric vector}
  \item{y}{ numeric vector or matrix}
}
\details{
  Does not support missing values. If y is a column of a vector,
the transpose must be used.
}
\value{
  \item{distancia}{ numeric value representing the Euclidean distance between
x and y, or a row matrix representing the Euclidean distance between x and
each column of y.
}
}
\author{Caroline Rodriguez and Edgar Acuna }
\examples{
#---- Calculating distances
x=rnorm(4)
y=matrix(rnorm(12),4,3)
distancia(x,t(y[,1]))
distancia(x,y)

}
\keyword{math}


