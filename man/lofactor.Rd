\name{lofactor}
\alias{lofactor}
\title{ Local Outlier Factor }
\description{ A function that finds the local outlier factor (Breunig et al.,2000) of
the matrix "data" using k neighbors. The local outlier factor (LOF) is a measure of outlyingness
that is calculated for each observation. The user decides whether or not an observation 
will be considered an outlier based on this measure. The LOF takes into consideration
the density of the neighborhood around the observation to determine its outlyingness.
  }
\usage{
lofactor(data, k)
}
\arguments{
  \item{data}{ The data set to be explored}
  \item{k}{ The kth-distance to be used to calculate the LOF's.}
}
\details{
  The LOFs are calculated over a range of values, and the max local outlier factor
is determined over this range.
}
\value{
  \item{lof}{ A vector with the local outlier factor of each observation}
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
