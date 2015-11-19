\name{mo4}
\alias{mo4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The fourth moment of a multivariate distribution }
\description{
This function computes the fourth moment of a multivariate distribution.
This result is used later in the mardia's test for multivariate normality.
}
\usage{
mo4(data)
}

\arguments{
  \item{data}{ The dataset containing the features of the multivariate vector for which the 
fourth moment will be computed. Do not include the class attribute for supervised datasets }
}
\value{
Returns the fourth moment.}
\author{ Edgar Acuna }
\seealso{ \code{\link{mo3}}, \code{\link{mardia}} }
\examples{
data(iris)
mo4(iris[,-5])
}
\keyword{ multivariate }
