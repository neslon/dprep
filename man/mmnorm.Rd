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
  \item{data}{The dataset to be normalized, including classes}
  \item{minval}{The minimun value of the transformed range}
  \item{maxval}{The maximum value of the transformed range}
}
\details{
  Min-max normalization subtracts the minimum value of an attribute from each value
of the attribute and then divides the difference by the range of the attribute.
These new values are multiplied by the new range of the attribute
and finally added to the new minimum value of the attribute.
These operations transform the data into a new range, generally [0,1]. The function
removes classes (assuming they are in last column) before normalization, and 
returns a normalized data set, complete with classes. Uses the function scale from the base package.

}
\value{
  \item{zdata3}{ The normalized dataset}
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
\keyword{Normalization}
