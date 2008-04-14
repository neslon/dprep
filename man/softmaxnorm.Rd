\name{softmaxnorm}
\alias{softmaxnorm}
\title{Softmax Normalization}
\description{
This is a function that applies softmax normalization to a matrix or dataframe.
}
\usage{
softmaxnorm(data)
}
\arguments{
  \item{data}{ The datset to be normalized}
}
\details{
  This normalization is so called because it reaches "softly" towards
maximum and minimum value, never quite getting there. The
transformation is more or less linear in the middle range, and has a
nonlinearity at both ends. The output range covered is [0,1].  The
algorithm removes the classes of the dataset before normalization
and replaces them at the end to form the matrix again.
}
\value{
  \item{softdata}{ original matrix normalized}
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
#---- Softmax Normalization----
data(sonar)
sonar.sftnorm=softmaxnorm(sonar)
op=par(mfrow=c(2,1))
plot(sonar[,1])
plot(sonar.sftnorm[,1])
par(op)
}
\keyword{manip}
