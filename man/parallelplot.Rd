\name{parallelplot}
\alias{parallelplot}
\title{Parallel Coordinate Plot}
\description{Constructs a parallel coordinate plot for a data set with classes
}
\usage{
parallelplot(x, name = "", comb = 0, class = 0, obs = rep(0, 0), 
col = 2, lty = 1, ...)
}
\arguments{
  \item{x}{ matrix of numerical values with classes in last column}
  \item{name}{ name of data set as will appear in the graph title}
  \item{comb}{ if comb=0, all distinct combinations of columns are graphed, else
should be an integer that represents the number of one of the possible 
combinations for the columns of this matrix.}
  \item{class}{ value representing the class number to which the plot should be limited }
  \item{obs}{ list of one or more row numbers that are to be highlighted in the plot}
  \item{col}{ value that provides a choice of color for the plot if limited to one class}
  \item{lty}{ value that provides a choice of line width for the plot if limited to one class}
  \item{\dots}{ additional arguments for the matplot function }
}
\value{
A parallel coordinate plot of the data is produced
}
\details{
This plot is not recommended for a large number of features (say more than 50)
}

\references{Wegman, E. (1990), Hyperdimensional data analysis using parallel coordinates, 
	Journal of the American Statistical Association,85,664-675
}
\author{Caroline Rodriguez}
\seealso{\code{\link{starcoord}},  \code{\link{surveyplot}} }
\examples{
#---Parallel Coordinate Plot----
data(bupa)
parallelplot(bupa,"Bupa Dataset")
parallelplot(bupa,"Bupa Dataset",comb=1)
#parallelplot(bupa,"Bupa Dataset",comb=1,c(1,22,50))
}
\keyword{hplot}

