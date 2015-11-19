\name{radviz2d}
\alias{radviz2d}
\title{ Radial Coordinate Visualization}
\description{
  Radviz is a radial spring-based visualization that permits the visualization
of n-dimensional datasets. Data attributes are equidistantly distributed along 
the circumference of a circle. Each data item is virtually 
connected to a spring that starts at the circle perimeter and ends on the data item. 
Each spring pulls the item with a force proportional to the item attribute value. 
Depending on the value of each attribute, the forces of the springs project 
each data item to a position inside the circle where the sum of the spring forces 
is equal to zero. 
}
\usage{
radviz2d(dataset, name = "")
}
\arguments{
  \item{dataset}{ The dataset to be visualized. }
  \item{name}{ The name of the dataset to be used in the graph title. }
}
\details{
Some features of this visualization are:
l) Points where all dimensional values have approximately the same value will lie 
close to the center.
2) If dimensional points lie opposite each other on the circle and have similar 
values than points will lie near the center.
3) If 1 or 2 dimensional values are greater, points will lie closer to those 
dimensional points.
4) Where a point will lie depends on the layout of the particular dimensions around 
the circle.
5) This is a non-linear projection from N-dimensions down to 2 dimensions
6) Certain symmetries of the data will be preserved.

The function assumes the class labels are in the last column. Class column may be
either a numeric vector or a factor.
}
\value{
  A Radviz visualization of the original dataset is returned.

}
\references{ Ankerst M., Keim D. A., Kriegel H.-P. Circle Segments: A Technique for Visually
Exploring Large Multidimensional Data Sets, IEEE Visualization, 1996.

K.A. Olsen, R.R. Korfhage, K.M. Sochats, M.B. Spring and J.G. Williams. Visualisation of a
Document Collection: The VIBE System, Information Processing and Management, Vol. 29, No. 1, pp.
69-81, Pergamon Press Ltd, 1993.
}
\author{ Caroline Rodriguez }
\seealso{\code{\link{starcoord}},  \code{\link{surveyplot}}, \code{\link{parallelplot}}  }
\note{ Prior to visualizing, the values of each attribute are usually standardized to the interval 
[0, 1] to make all the attributes equally important in "pulling" the data point. If one 
attribute value is much larger than the values of the other attributes, then the point will lie 
close to the point on the circumference of the circle which corresponds to this attribute. 
The visualization of a given data set, and also its usefulness, largely depends on the selection 
of visualized attributes and their ordering around the circle perimeter. The total number of 
possible orderings of m attributes is factorial(m), but some of them are equivalent up to a rotation or image 
mirroring. Hence, it can be shown that the total number of different projections with m attributes 
is factorial(m-1)/2.
}
\examples{
data(iris)
radviz2d(iris,"Iris")
}
\keyword{Visualization}
