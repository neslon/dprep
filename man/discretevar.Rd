\name{discretevar}
\alias{discretevar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Performs Minimum Entropy discretization for a given attribute}
\description{
 This function carries out ME discretization for a given attribute of a 
dataset}
}
\usage{
discretevar(data, var, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The name of the dataset}
  \item{var}{ The column were is located the attribute to be discretized}
  \item{n}{ number of rows of the dataset}
  \item{p}{ number of columns of the dataset}
}
\author{ Luis Daza}
\seealso{\code{\link{disc.mentr}}}
\keyword{manip}% at least one, from doc/KEYWORDS
