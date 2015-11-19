\name{acugow}
\alias{acugow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gower distance from a vector to a matrix
}
\description{
This function finds out the gower distance between a vector and a 
matrix
}
\usage{
acugow(x, data, vnom = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A Vector of attributes values 
}
  \item{data}{
A matrix dataset
}
  \item{vnom}{
A vector indicating the columns with nominal attributes in the matrix dataset
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{matdist }{a matrix containing the components of the distance vector from x to each row of data}
\item{dist }{a vector containing the distances from x to each row of data}
}
\author{
Edgar Acuna}

\seealso{\code{\link{reliefcont}}
}
\examples{
data(crx)
crx.imp=ce.impute(crx,"knn",nomatr=c(1,4,5,6,7,9,10,12,13),3)
acugow(crx.imp[1,-16],crx.imp[-1,-16],vnom=c(1,4,5,6,7,9,10,12,13))
}
\keyword{math}
