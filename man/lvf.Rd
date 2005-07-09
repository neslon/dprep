\name{lvf}
\alias{lvf}
\title{Las Vegas Filter}
\description{
  Las Vegas Filter uses a random generation of subsets and an 
inconsistency measure as the evaluation function to determine the
relevance of features.
}
\usage{
lvf(data, lambda, maxiter)
}
\arguments{
  \item{data}{ name of the discretized dataset}
  \item{lambda}{ threshold for the inconsistency}
  \item{maxiter}{ maximum number of iterations}
}
\details{
If the dataset has continuous variables, these must first be discretized. This
package includes four discretization methods. A value of lambda close to the 
inconsistency of the whole dataset yields a large number of selected features,
a large lambda yields few selected features.
}
\value{
  \item{bestsubset}{ the best subset of features }
}
\references{LIU, H. and SETIONO, R. (1996). A probabilistic approach to feature selectiona
filter solution. Proc. of the thirteenth International Conference of Machine
Learning, 319-337.
}
\author{Edgar Acuna}
\seealso{\code{\link{disc.ew}},\code{\link{inconsist}},\code{\link{finco}}}
\examples{
#---- LVF method ----
data(my.iris)
disciris=disc.ew(my.iris,1:6)
inconsist(disciris)
lvf(disciris,0,500)
}
\keyword{methods}
