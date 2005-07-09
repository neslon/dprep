\name{surveyplot}
\alias{surveyplot}
\title{Surveyplot}
\description{
  This function creates and displays a surveyplot of a dataset for a classification matrix
}
\usage{
surveyplot(datos, dataname = "", orderon = 0, class = 0, 
obs = rep(0, 0), lwd = 1)
}
\arguments{
  \item{datos}{ matrix of values for classification to be plotted}
  \item{dataname}{\code{dataname} name of data set to appear in plot title}
  \item{orderon}{\code{orderon} column number by which to order the dataset}
  \item{class}{ \code{class} class for which to limit plotting}
  \item{obs}{ \code{obs} list of observations to be highlighted}
  \item{lwd}{\code{lwd} value to control width of the line}
}
\value{
  returns a surveyplot of the data matrix
}
\details{
This plot is not recommended for a large number of features (say more than 50)
}
\references{Fayyad, et al. (2001) Information Visualization in Data Mining and Knowledge Discovery 
}
\author{Caroline Rodriguez}
\note{This plot is a mix between the survey plot presented in Fayyad and a permutation matrix.
}
\seealso{\code{\link{parallelplot}},  \code{\link{starcoord}} }
\examples{
#----Surveyplot examples
data(bupa)
surveyplot(bupa,"Bupa Dataset")
surveyplot(bupa,"Bupa Dataset",orderon=1,obs=c(6,74,121))
}
\keyword{hplot}
