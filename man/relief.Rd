\name{relief}
\alias{relief}
\title{RELIEF Feature Selection}
\description{
This function implements the RELIEF feature selection algorithm.
}
\usage{
relief(data, nosample, threshold,vnom)
}
\arguments{
  \item{data}{ the dataset for which feature selection will be carried out}
  \item{nosample}{ number of instances drawn from the original dataset}
  \item{threshold}{ the cutoff point to select the features}
 \item{vnom}{ a vector containing the indexes of the nominal features}
}
\details{
  The general idea of this method is to choose the features that can be most
distinguished between classes. These are known as the relevant features. At 
each step of an iterative process, an instance x is chosen at random from
the dataset and the weight for each feature is updated adcording to the 
distance of x to its Nearmiss and NearHit.
}
\value{
  \item{relevant}{ a table that gives the frequency with which the
feature was selected as relevant over the trials performed, and
the average weight of the feature.}
 \item{a plot}{ a plot of the weights of the features}
}
\references{KIRA, K. and RENDEL, L. (1992). The Feature Selection Problem : Traditional
Methods and a new algorithm. Proc. Tenth National Conference on Artificial
Intelligence, MIT Press, 129-134.

KONONENKO, I., SIMEC, E., and ROBNIK-SIKONJA, M. (1997). Overcoming
the myopia of induction learning algorithms with RELIEFF. Applied Intelligence
Vol7, 1, 39-55.
}
\author{ Edgar Acuna}
\examples{
##---- Feature Selection ---
data(my.iris)
relief(my.iris,150,0.01)
}
\keyword{methods}
