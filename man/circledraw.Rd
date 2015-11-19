\name{circledraw}
\alias{circledraw}
\title{circledraw}
\description{
  This function draws a circle using the polygon function from the graphics package. It is
an auxiliary function used by radviz2d.
}
\usage{
circledraw (numpts = 200, radius = 1)
}
\arguments{
  \item{numpts}{ Number of edges of the polygon, default is 200.}
  \item{radius}{Radius of the circle to be drawn, default is 1.}
  }
\value{
  Displays a circle of radius = radius.
}
\details{
A circle of a specified radius is drawn by the polygon function of the graphics library
by constructing a polygon with numpts number of edges. It is intended to be an auxiliary
function for the radviz2d visualization.}

\author{Caroline Rodriguez}

\examples{
#----Circledraw examples
circledraw()
}
\keyword{misc}
