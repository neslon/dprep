\name{disc.ef}
\alias{disc.ef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Discretization using the method of equal frequencies}
\description{
  Unsupervised discretization using intervals of equal frequencies
}
\usage{
disc.ef(data, varcon, k,out=c("symb","num"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataset to be discretized}
  \item{varcon}{A vector containing the continuous features}
  \item{k}{ The number of intervals to be used}
\item{out}{To get the discretized dataset in a numerical format write "num". To get
the discretized in an interval format write "symb"}
}
\value{
Returns a new data matrix with discretized values.
}
\references{
Kantardzic M. (2003). Data Mining: Concepts, Models, methods, and Algorithms. 
John Wiley. New York.}
\author{Edgar Acuna}
\seealso{\code{\link{disc.1r}}, \code{\link{disc.ew}},\code{\link{chiMerge}}}
\examples{
#Discretization using the equal frequency method
data(bupa)
bupa.disc.ef=disc.ef(bupa,1:6,8,out="symb")
}
\keyword{Discretization}

