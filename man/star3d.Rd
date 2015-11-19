\name{star3d}
\alias{star3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Data Visuaization using star coordinates in three dimensions 
}
\description{
This function perform data visulization using cooordinates in three dimensions.
Rotation and zooming is possible calling to the rgl library
}
\usage{
star3d(data, vars = NULL, scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataset to be visualized 
}
  \item{vars}{
The variables to be scaled}
  \item{scale}{the scale factor}
}
\author{
Edgar Acuna}
\seealso{\code{\link{starcoord}}
}
\examples{
\dontrun{
data(vehicle)
star3d(vehicle)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory
\keyword{Visualization}

