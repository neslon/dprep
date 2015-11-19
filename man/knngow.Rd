\name{knngow}
\alias{knngow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ K-nn classification using Gower distance
}
\description{
This function performs classification using the k- nearest neighbors but
using Gower distance rather than Euclidean distance. It is recommended
if the dataset has niminal and continuous attributes.
}
\usage{
knngow(train, test, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{
A matrix containing the training dataset.
}
  \item{test}{
A matrix containing the test dataset.
}
  \item{k}{
The number of neighbors to be used. 
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{predclass}{A vector of predited classes}
}
\author{Edgar Acuna}

\seealso{\code{\link{acugow}}
}
\examples{
\dontrun{data(crx)
knngow(crx,crx,3)}
}
\keyword{ classification }

