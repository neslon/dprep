\name{mo4}
\alias{mo4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The fourth moment of a multivariate distribution }
\description{
This function computes the fourth momemnt of a multivariate distribution.
This result is used later in the mardia's test for multivariate normality
}
\usage{
mo4(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The dataset containing the features of the mutlivariate vector which
fourth momment will be computed}
}
\value{
Returns the fourth moment}
\author{ Edgar Acuna }
\seealso{ \code{\link{mo3}}, \code{\link{mardia}} }
\examples{
data(my.iris)
mo4(my.iris)
}
\keyword{multivariate}% at least one, from doc/KEYWORDS
