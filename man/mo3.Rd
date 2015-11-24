\name{mo3}
\alias{mo3}
\title{ The third moment of a multivariate distribution }
\description{
 This function computes the third moment of a multivariate normal distribution.
This result is used later on the Mardia's test for multivariate normality
}
\usage{
mo3(data)
}
\arguments{
  \item{data}{ The dataset containing the features of the multivariate vector for 
which the third moment will be computed. Do not include the class attribute for supervised datasets }
}
\value{
  \item{mo3 }{The third moment of the multivariate distribution} 
}
\author{ Edgar Acuna}
\seealso{ \code{\link{mo4}}, \code{\link{mardia}} }
\examples{
\dontrun{data(iris)
mo3(iris[,-5])
}
}
\keyword{multivariate}