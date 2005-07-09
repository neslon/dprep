\name{mmnorm}
\alias{mmnorm}
\title{ Min-max normalization}
\description{
  This is a function to apply min-max normalization to a matrix or dataframe.
}
\usage{
mmnorm(data,minval=0,maxval=1)
}
\arguments{
  \item{data}{ the dataset to be normalized, including classes}
  \item{minval} {the minimun value of the transformed range}
  \item{maxval} {the maximum value of the transformed range}
}
\details{
  Min-max normalization subtracts the minimum value of an attribute from each value
of the attribute and then divides the difference by the range of the attribute.
These new values are multiplied by the new range of the attribute
and finally added to the new minimum value of the attribute.
These operations transform the data into a new range, generally [0,1].

Removes classes before normalization, and returns normalized data set
complete with classes rejoined.
}
\value{
  \item{zdata3}{ the original dataset normalized by min-max normalization.}
}
\references{ Hann, J., Kamber, M. (2000). Data Mining: Concepts and Techniques. 
 Morgan Kaufman Publishers.
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
#---- Min-Max Normalization----
data(ionosphere)
ionos.minmax=mmnorm(ionosphere)
op=par(mfrow=c(2,1))
plot(ionosphere[,1])
plot(ionos.minmax[,1])
par(op)
}
\keyword{manip}
