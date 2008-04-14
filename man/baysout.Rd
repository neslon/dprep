\name{baysout}
\alias{baysout}
\title{
Outlier detection using Bay and Schwabacher's algorithm.
}
\description{
  This function implements the algorithm for outlier detection found
in Bay and Schwabacher(2003). The algorithm assigns an outlyingness measure to each 
observation and returns the indexes of those observations having the largest measures. 
The number of outliers to be returned is specified by the user.
}
\usage{
baysout(D, blocks = 5, k = 3, num.out = 10)
}
\arguments{
  \item{D}{ the dataset under study}
  \item{blocks}{ the number of sections in which to divide the entire dataset. It  must be 
          at least as large as the number of outliers requested.
}
  \item{k}{ the number of neighbors to find for each observation}
  \item{num.out}{ the number of outliers to return}
}
\value{ 
  \item{num.out}{ Returns a two column matrix containing the indexes of the observations
with the top num.out outlyingness measures. A plot of the top candidates
and their measures is also displayed.
}
}
\references{
Bay, S.D., and Schwabacher (2003). Mining distance-based 
outliers in near linear time with randomization and a simple pruning rule.
}
\author{ Caroline Rodriguez(2004). Modified by Elio Lozano (2005)}
\examples{
#---- Outliers detection using the Bay's algorithm----
data(bupa)
bupa.out=baysout(bupa[bupa[,7]==1,1:6],blocks=10,num.out=10)
}
\keyword{methods}
