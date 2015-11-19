\name{parallelplot}
\alias{parallelplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Parallel Coordinate Plot}
\description{
  Constructs a parallel coordinate plot for a data set with classes in last column.
}
\usage{
parallelplot(x, name = "", comb = -1, class = 0, obs = rep(0, 0), col = 2, lty = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x}{ A matrix of numerical values with classes in last column}
  \item{name}{ The name of data set as will appear in the graph title}
  \item{comb}{ An integer that represents the number of one of the possible 
combinations for the columns of this matrix.}
  \item{class}{ A value representing the class number to which the plot should be limited }
  \item{obs}{ A list of one or more row numbers that are to be highlighted in the plot}
  \item{col}{ A value that provides a choice of color for the plot (if plotting only one class)}
  \item{lty}{ A value that provides a choice of line width for the plot (if plotting only one class)}
  \item{\dots}{ Additional arguments for the matplot function }
}
\details{
  This plot is not recommended for a large number of features (say more than 50).
If comb=0, all distinct combinations of columns are graphed. If comb=-1 (default), the attributes are 
plotted in their original order, else comb should be equal to an integer that represents 
the number of one of the possible combinations for the columns of this matrix.
}
\value{
  A parallel coordinate plot of the data is produced.
}
\references{Wegman, E. (1990), Hyperdimensional data analysis using parallel coordinates, 
	Journal of the American Statistical Association,85,664-675}
\author{ Caroline Rodriguez}
\seealso{\code{\link{starcoord}},  \code{\link{surveyplot}} }
\examples{
#---Parallel Coordinate Plot----
data(bupa)
parallelplot(bupa,"Bupa Dataset")
parallelplot(bupa,"Bupa Dataset",comb=0)
#parallelplot(bupa,"Bupa Dataset",comb=1,c(1,22,50))
}
\keyword{ Visualization}
