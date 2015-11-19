\name{combinations}
\alias{combinations}
\title{Constructing distinct permutations }
\description{
  A function for constructing the minimal set of permutations 
of the elements in the sequence 1:numcol as described by Wegman in 
Hyperdimensional Data Analysis(1999)
}
\usage{
combinations(numcol)
}
\arguments{
  \item{numcol}{  A value representing the number of columns in a matrix}
}
\value{
  A matrix in which each column represents a distinct permutation of the sequence 1:numcol 
}
\references{Wegman, E. (1990), Hyperdimensional data analysis using parallel 
coordinates, Journal of the American Statistical Association, 85, 664-675.
}
\author{Caroline Rodriguez}
\keyword{misc}
