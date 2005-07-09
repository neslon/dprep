\name{lofactor}
\alias{lofactor}
\title{ Local Outlier Factor }
\description{ A function that finds the local outlier factor (Breunig et al.,2000) of
the matrix "data" with k neighbors. The local outlier factor (LOF) is a measure of outlyingness
that is calculated for each observation. The user decides whether or not an observation 
will be considered an outlier or not based on this measure. The LOF takes into consideration
the density of the neighborhood around the observation to determine its outlyingness.
  }
\usage{
lofactor(data, k)
}
\arguments{
  \item{data}{ the data set to be explored}
  \item{k}{ the value that determines the k-distance to be used to calculate
the LOF's.}
}
\details{
  The LOF are calculated over a range of values, and the max local outlier factor
is determined over this range.
}
\value{
  \item{lof}{ a vector with the local outlier factor of each observation}
}
\references{ Breuning, M., Kriegel, H., Ng, R.T, and Sander. J. (2000). 
LOF: Identifying density-based local outliers. In Proceedings of the ACM SIGMOD 
International Conference on Management of Data.}
\author{Caroline Rodriguez}
\examples{
#---- Detecting the top 10 outliers using the LOF algorithm----
data(my.iris)
iris.lof=lofactor(my.iris,10)
}
\keyword{methods}
