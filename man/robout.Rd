\name{robout}
\alias{robout}
\title{Outlier Detection with Robust Mahalonobis distance}
\description{
  This function finds out the outliers of a dataset using 
robust versions of the Mahalanobis distance.
}
\usage{
robout(data, nclass, meth = c("mve", "mcd"), rep = 10, 
plot = TRUE)
}
\arguments{
  \item{data}{ the dataset for which outlier detection will be carried out.}
  \item{nclass}{ integer value that represents the class to detect for outliers}
  \item{meth}{ method used to compute the Mahalanobis distance, "mve"=minimum
volume estimator, "mcd"=minimum covariance determinant} 
  \item{rep}{ number of repetitions}
  \item{plot}{ boolean value to turn on or off the scatter plot of the 
Mahalanobis distances}
}
\details{
Requires uses cov.rob function from the MASS library.
}
\value{
  \item{top1} { index of observations identified as top outliers by 
frequency of selection}
  \item{topout}{ index of observations identified as possible outliers 
by outlyingness measure}
  \item{outme}{ index of observations and their outlyingness measures}
}
\references{
Rousseeuw, P, and Leroy, A. (1987). Robust Regression and outlier detection. John Wiley & Sons. New York.

Atkinson, A. (1994). Fast very robust methods for the detection of 
multiple outliers. Journal of the American Statistical Association, 89:1329-1339.
}
\author{Edgar Acuna}
\examples{
#---- Outlier Detection in bupa-class 1 using MCD
data(bupa)
robout(bupa,1,"mcd")
}
\keyword{methods}
