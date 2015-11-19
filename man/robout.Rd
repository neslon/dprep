\name{robout}
\alias{robout}
\title{Outlier Detection with Robust Mahalonobis distance}
\description{
  This function finds the outliers of a dataset using 
robust versions of the Mahalanobis distance.
}
\usage{
robout(data, nclass=0, meth = c("mve", "mcd"), rep = 10, 
plot = TRUE)
}
\arguments{
  \item{data}{ The dataset for which outlier detection will be carried out.}
  \item{nclass}{ An integer value that represents the class to detect for outliers. By default nclass=0 meaning
the column of classes it is not used.}
  \item{meth}{ The method used to compute the Mahalanobis distance, "mve"=minimum
volume estimator, "mcd"=minimum covariance determinant} 
  \item{rep}{ Number of repetitions}
  \item{plot}{ A boolean value to turn on and off the scatter plot of the 
Mahalanobis distances}
}
\details{
It requires the use of the cov.rob function from the MASS library.
}
\value{
  \item{top1}{Index of observations identified as top outliers by  frequency of selection}
  \item{topout}{Index of observations identified as possible outliers 
by outlyingness measure}
  \item{outme}{Index of observations and their outlyingness measures}
}
\references{
Rousseeuw, P, and Leroy, A. (1987). Robust Regression and outlier detection. John Wiley & Sons. New York.

Atkinson, A. (1994). Fast very robust methods for the detection of 
multiple outliers. Journal of the American Statistical Association, 89:1329-1339.
}
\author{Edgar Acuna}
\seealso{ \code{\link{robout}} }
\examples{
\dontrun{#---- Outlier Detection in bupa-class 1 using MCD
data(bupa)
robout(bupa,1,"mcd")
}}
\keyword{Outlier Detection}
