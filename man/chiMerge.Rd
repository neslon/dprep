\name{chiMerge}
\alias{chiMerge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Discretization using the Chi-Merge method}
\description{This function performs supervised discretization using the Chi Merge method.
}
\usage{
chiMerge(data, varcon, alpha = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The name of the dataset to be discretized}
  \item{varcon}{ Vector of continuous variables}
  \item{alpha}{ The significance level}
}
\details{
In case of datasets containing negative values apply first a range normalization
to change the range of the attributes values to an interval containing positive values.
The discretization process becomes slow when the number of variables increases (say for more 
than 100 variables).  
}
\value{
  \item{discdata }{A new data matrix containing the discretized features}
}
\references{
Kantardzic M. (2003). Data Mining: Concepts, Models, methods, and Algorithms. 
John Wiley. New York.
}
\author{ Edgar Acuna, Jaime Porras, and  Carlos Lopez}
\seealso{ \code{\link{disc.ef}}, \code{\link{disc.ew}},\code{\link{disc.1r}},\code{\link{disc.mentr}}}
\examples{
#-----Discretization using the ChiMerge method
data(my.iris)
iris.disc=chiMerge(my.iris,1:4,alpha=0.05)
#-----Applying chiMerge a dataset containing negative values 
#data(ionosphere)
#normionos=rangenorm(ionosphere,"mmnorm")
#ionos.disc=chiMerge(normionos,1:32)
}

\keyword{ manip }
