\name{unor}
\alias{unor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Auxiliary function for performing Holte's 1R discretization 
}
\description{
This function is called by the disc.1r function 
}
\usage{
unor(a, binsize, out = c("symb", "num"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{a}{a is a two column  matrix where the first column contains the values to be discretized and
the second column contains the class labels.}
\item{binsize}{the minimum number of attributes values  in each bin.}
\item{out}{To get the discretized data in numerical format enter "nun". To get
the discretized data in interval format enter "symb".} 
}
\value{
Returns the discretized values of the first column of the matrix a.
}
\author{Edgar Acuna}
\seealso{ \code{\link{disc.1r}}}
\keyword{Discretization}

