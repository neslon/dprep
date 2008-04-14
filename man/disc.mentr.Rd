\name{disc.mentr}
\alias{disc.mentr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Discretization using the minimum entropy criterion}
\description{
  This function discretizes the continuous attributes of a data frame 
using the minumum entropy criterion with the minimum description length 
as stopping rule.
}
\usage{
disc.mentr(data, vars)
}

\arguments{
  \item{data}{ The name of the dataset to be discretized}
  \item{vars}{ A vector containing the indices of the columms 
to be discretized and column containing the classes}
}
\value{
Returns a matrix containing only discretized features.}
\references{ 
Dougherty, J., Kohavi, R., and Sahami, M. (1995). Supervised and unsupervised discretization 
of continuous features. ML-95.
}
\author{Luis Daza}
\seealso{ \link{disc.1r}, \link{disc.ew},\link{disc.ef},\link{chiMerge} }
\examples{
data(my.iris)
iris.discme=disc.mentr(my.iris,1:5)
}
\keyword{manip}
