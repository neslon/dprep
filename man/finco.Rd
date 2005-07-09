\name{finco}
\alias{finco}
\title{FINCO Feature Selection Algorithm}
\description{
This function selects features using the FINCO algorithm. The dataset
must contain only discretized values.
}
\usage{
finco(data, level)
}
\arguments{
  \item{data}{name of the dataset containing the discretized values}
  \item{level}{minimum inconsistency level}
}
\details{
  The level value must be greater than the inconsistency of the whole dataset, which first
 must be discretized. The function inconsist included in this library  computes inconsistencies.
 A small value of level yields a greater number of selected features.
}
\value{
  \item{varselec}{ index of selected features}
  \item{inconsis}{ inconsistency rates of the selected features}
}
\author{Edgar Acuna}
\references{Acuna, E , (2003) A comparison of filters and wrappers for feature selection in supervised classification.
Proceedings of  the Interface 2003 Computing Science and Statistics. Vol 34.

Acuna, E., Coaquira, F. and Gonzalez, M. (2003). A comparison of feature selection 
procedures for classifiers based on kernel density estimation. Proc. of the Int. Conf. on Computer, 
Communication and Control technologies, CCCT03. VolI. p. 468-472. Orlando, Florida. 
}
\seealso{\code{\link{inconsist}},\code{\link{lvf}}}
\examples{
#---- Feature Selection with FINCO
data(my.iris)
disciris=disc.ew(my.iris,1:6)
inconsist(disciris)
finco(disciris,0.05)
}
\keyword{methods}
