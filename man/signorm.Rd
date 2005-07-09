\name{signorm}
\alias{signorm}
\title{Sigmoidal Normalization}
\description{
  Function that performs sigmoidal normalization.
}
\usage{
signorm(data)
}
\arguments{
  \item{data}{ the dataset to be normalized, including classes}
}
\details{
  This method transforms the input data nonlinearly into [-1,1] using
a sigmoid function. The original data is first centered about the 
mean, and then mapped to the almost linear region of the sigmoid.
Is especially appropriate when outlying values are present.

Removes classes before normalization, and returns normalized data set
complete with classes rejoined.
}
\value{
  \item{sigdata}{ original dataset normalized}
  }
\references{ Hann, J., Kamber, M. (2000). Data Mining: Concepts and Techniques. 
 Morgan Kaufman Publishers.
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
#---- Sigmoidal Normalization ---
data(vehicle)
vehicle.signorm=signorm(vehicle)
op=par(mfrow=c(2,1))
plot(vehicle[,1])
plot(vehicle.signorm[,1])
par(op)
}
\keyword{manip}