\name{maxlof}
\alias{maxlof}
\title{ Detection of multivariate outliers using the LOF algorithm}
\description{
  A function that detects multivariate outliers using the local outlier factor for a matrix
over a range of neighbors called Minpts.
}
\usage{
maxlof(data, name = "", minptsl = 10, minptsu = 20)
}
\arguments{
  \item{data}{ dataset for outlier detection}
  \item{name}{ name to be given to the dataset, used in the graph title.}
  \item{minptsl}{ lower bound for the range of number of neighbors}
  \item{minptsu}{ upper bound for the range of number of neighbors}
}
\details{
  Calls on the function "lofactor" to compute the local
outlier factor for each number of neighbors in the range
[minptsl, minptsu]. Also displays a plot of the factors for
each observation of the dataset. In the plot, the user should
seek to identify observations with large gaps between outlyingness
measures. These would be the candidate for outliers.
}
\value{
  \item{maxlofactor}{ a vector containing the index of each observation
of the dataset and the corresponding local outlier factor.}
}
\references{Breuning, M., Kriegel, H., Ng, R.T, and Sander. J. (2000). 
LOF: Identifying density-based local outliers. In Proceedings of the 
ACM SIGMOD International Conference on Management of Data.
}
\author{Caroline Rodriguez}
\examples{
#Detecting top 10 outliers in Breastw-class1 using the LOF algorithm
data(breastw)
breastw1.lof=maxlof(breastw[breastw[,10]==1,],name="",30,40)
breastw1.lof[order(breastw1.lof,decreasing=TRUE)][1:10]
}
\keyword{methods}
