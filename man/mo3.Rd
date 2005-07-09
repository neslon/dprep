\name{mo3}
\alias{mo3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The third momment of a multivariate distribution }
\description{
 This function computes the third moment of a multivariate normal distribution.
This result is used later on the Mardia's test for multivariate normality
}
\usage{
mo3(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The dataset containing the features of the multivariate vector 
which third momment will be computed }
}
\value{
  \item{mo3 }{The third momment of the multivariate distribution} 
}
\author{ Edgar Acuna}
\seealso{ \code{\link{mo4}}, \code{\link{mardia}} }
\examples{
data(my.iris)
mo3(my.iris)
}
\keyword{multivariate}
