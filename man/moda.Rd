\name{moda}
\alias{moda}
\title{Calculating the Mode}
\description{
This function calculates the mode of a vector.
}
\usage{
moda(x, na.rm = TRUE)
}
\arguments{
  \item{x}{ A numeric vector }
  \item{na.rm}{ A Boolean value that indicates the presence of missing values. }
}
\details{
The function returns the mode or modes of a vector. If a tie exists,
all values that are tied are returned.
}
\value{
  \item{moda}{ A numeric value representing the mode of the vector}
  }
\author{Caroline Rodriguez and Edgar Acuna }
\examples{
#---- Calculating the mode ----
x=c(1,4,2,3,4,6,3,7,8,5,4,3)
moda(x)
}
\keyword{math}
