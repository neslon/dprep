\name{disc.1r}
\alias{disc.1r}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Discretization using the Holte's 1R method }
\description{
  This function performs supervised discretization using the Holte's 1R method
}
\usage{
disc.1r(data, convar, binsize = 15, out=c("symb","num"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The name of the dataset to be discretized}
  \item{convar}{A vector containing the continuous features}
  \item{binsize}{ The number of instances per bin}
\item{out}{To get the discretized dataset in a numerical format write "num". To get
the discretized in an interval format write "symb"}
}
\value{
Returns a new data matrix with discretized values
}
\references{
Kantardzic M. (2003). Data Mining: Concepts, Models, methods, and Algorithms. 
John Wiley. New York. }
\author{Edgar Acuna}

\seealso{\code{\link{disc.ew}},\code{\link{disc.ef}}, \code{\link{chiMerge}}, \code{\link{disc.mentr}}}
\examples{
#-----Discretization using the Holte's 1r method
data(bupa)
disc.1r(bupa,1:6,out="symb")}
\keyword{Discretization}

