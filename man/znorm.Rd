\name{znorm}
\alias{znorm}
\title{Z-score normalization}
\description{
This is a function to apply z-Score normalization to a matrix or dataframe.
}
\usage{
znorm(data)
}
\arguments{
  \item{data}{ The dataset to be normalized, including classes}
}
\details{
  By using this type of normalization, the mean of the transformed set
of data points is reduced to zero by subtracting the mean of each
attribute from the values of the attributes and dividing the result
by the standard deviation of the attribute. Uses the function
scale found in the base library.

 Removes classes before normalization, and returns normalized data set
complete with classes rejoined.
}
\value{
  \item{zdata}{ the normalized data set}
}
\references{Hann, J., Kamber, M. (2000). Data Mining: Concepts and Techniques. 
 Morgan Kaufman Publishers.
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
##---- Z-norm normalization ----
data(diabetes)
diab.znorm=znorm(diabetes)
op=par(mfrow=c(2,1))
plot(diabetes[,1])
plot(diab.znorm[,1])
par(op)
}

\keyword{Normalization}
