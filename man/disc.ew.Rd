\name{disc.ew}
\alias{disc.ew}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Discretization using the equal width method }
\description{
Unsupervised discretization using intervals of equal width. The widths are computed 
using the   Scott's formula
}
\usage{
disc.ew(data, varcon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The name of the dataset containing the attributes to be discretized}
  \item{varcon}{A vector containing the indexes of the attributes to be discretized}
}
\value{
Returns a new data matrix with discretized values}
\references{ 
Venables, W.N., and Ripley, B.D. (2002). Modern Applied Statistics with S. Fourth edition, Springer}
\author{ Edgar Acuna}
\seealso{\code{\link{disc.ef}}, \code{\link{disc.1r}},\code{\link{chiMerge}},\code{\link{disc.mentr}} }
\examples{
#----Discretization using the equal frequency method
data(bupa)
disc.ew(bupa,1:6)
}
\keyword{manip}
