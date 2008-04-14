\name{mahaout}
\alias{mahaout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multivariate outlier detection through the boxplot of the Mahalanobis distance}
\description{
This function finds multivariate outliers by constructing a
boxplot of the Mahalanobis distance of all the instances. 
}
\usage{
mahaout(data, nclass, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Name of the dataset}
  \item{nclass}{Number of the class to check for outliers}
  \item{plot}{ Logical value. If plot=T a plot of the mahalanobis distance is drawn}
}
\details{
  uses cov.rob function from the MASS library
}
\value{
Returns a list of top outliers according to their Mahalanobis distance and a list of 
all the instances ordered according to their Mahalanobis distance.

If Plot=T, a plot of the instances ranked by their Mahalanobis distance is provided. 
}
\references{ 
Rousseeuw, P, and Leroy, A. (1987). Robust Regression and outlier detection. John Wiley & Sons. New York.
 }
\author{Edgar Acuna}
\seealso{ \code{\link{robout}} }
\examples{
#---- Detecting outliers using the Mahalanobis distance----
data(bupa)
mahaout(bupa,1)
}

\keyword{methods}
