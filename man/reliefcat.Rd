\name{reliefcat}
\alias{reliefcat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Feature selection by the Relief Algorithm for datasets with only nominal features}
\description{
This function applies the RELIEF Algorithm to datasets containing nominal attributes.  
}
\usage{
reliefcat(data, nosample, threshold, vnom)
}
\arguments{
  \item{data}{ The name of the dataset }
  \item{nosample}{ The size of the sample drawn and used to update the relevance of each feature}
  \item{threshold}{ The threshold for choosing the relevant features}
  \item{vnom}{ A vector of indices indicating the nominal features}
}
\author{ Edgar Acuna}
\seealso{\code{\link{relief}}}
\keyword{methods }
