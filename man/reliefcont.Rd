\name{reliefcont}
\alias{reliefcont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Feature selection by the Relief for datasets only with continuous features}
\description{
This function applies Relief to datasets containing only 
continuous attrbutes.
}
\usage{
reliefcont(data, nosample, threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the name of the dataset }
  \item{nosample}{ the size of the sample drawn for updating the relevance of the features}
  \item{threshold}{ the threshold for choosing the relevant features.}
}
\author{ Edgar Acuna}
\seealso{\code{\link{relief}}}
\keyword{methods}% at least one, from doc/KEYWORDS
