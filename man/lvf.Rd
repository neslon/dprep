\name{lvf}
\alias{lvf}
\title{Las Vegas Filter}
\description{
  Las Vegas Filter uses a random generation of subsets and an 
inconsistency measure as the evaluation function to determine the
relevance of features in the dataset.
}
\usage{
lvf(data, lambda, maxiter)
}
\arguments{
  \item{data}{ Name of the discretized dataset}
  \item{lambda}{ Threshold for the inconsistency}
  \item{maxiter}{ Maximum number of iterations}
}
\details{
If the dataset has continuous variables, these must first be discretized. This
package includes four discretization methods. A value of lambda close to the 
inconsistency of the whole dataset yields a large number of selected features,
a large lambda yields few selected features.
}
\value{
  \item{bestsubset}{ The best subset of features }
}
\references{LIU, H. and SETIONO, R. (1996). A probabilistic approach to feature selection: a
filter solution. Proc. of the thirteenth International Conference of Machine
Learning, 319-337.
}
\author{Edgar Acuna}
\seealso{\code{\link{disc.ew}},\code{\link{inconsist}},\code{\link{finco}}}
\examples{
#---- LVF method ----
data(iris)
iris.discew=disc.ew(iris,1:4,out="num")
inconsist(iris.discew)
lvf(iris.discew,0,100)
}

\keyword{Feature Selection}
