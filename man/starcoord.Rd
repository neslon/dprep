\name{starcoord}
\alias{starcoord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The star coordinates plot}
\description{
  This function displays a star coordinates plot introduced by Kondogan (2001).
}
\usage{
starcoord(data, main = NULL, class = FALSE, outliers=NULL, vars = 0, 
scale = 1, cex = 0.8, lwd = 0.25, lty = par("lty"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataset}
  \item{main}{ The title of the plot }
  \item{class}{ This logical variable is TRUE for supervised data and FALSE for unsupervised data }
  \item{outliers}{ The instances to be highlighted as potential outliers }
  \item{vars}{ The variables to be scaled}
  \item{scale}{ The scale factor}
  \item{cex}{A numerical value giving the amount by which plotting text 
and symbols should be scaled. }
  \item{lwd}{ The width of the lines representing the axis}
  \item{lty}{ The type of the lines representing the axis}
}
\details{
This plot is not recommended for a large number of features (say more than 50).
}
\value{  
Returns a Star Coordinates Plot of the data matrix
}
\references{ 
E. Kandogan (2001). Visualizing multidimensional clusters, Trends, and Outliers, using 
star coordinates. Proceedings of KDD 2001.}
\author{Edgar Acuna and Shiyun Wen }
\seealso{\code{\link{parallelplot}},  \code{\link{surveyplot}} }
\examples{
data(vehicle)
starcoord(vehicle, main="Vehicle Dataset", class=TRUE, outliers=NULL,vars=0, scale=1,
cex=0.8, lwd = 0.25, lty = par("lty"))
}
\keyword{Visualization}
