\name{reliefcat}
\alias{reliefcat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Feature selection by the Relief for datasets only with nominal features}
\description{
This function applies the RELIEF on datasets containing either 
continuous or nominal attributes.  
}
\usage{
reliefcat(data, nosample, threshold, vnom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the name of the dataset }
  \item{nosample}{ the size of the sample drawn for updatin the relevance of each feature}
  \item{threshold}{ the threshold for choosing the relevant features}
  \item{vnom}{ a vector of indexes indicating the nominal features}
}
\author{ Edgar Acuna}
\seealso{\code{\link{relief}}}
\keyword{methods }% at least one, from doc/KEYWORDS
