\name{reliefcat}
\alias{reliefcat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Feature selection by the Relief Algorithm for datasets containing nominal features}
\description{
This function applies the RELIEF Algorithm to datasets containing nominal attributes.  
}
\usage{
reliefcat(data, nosample, threshold, vnom, repet)
}
\arguments{
  \item{data}{ The name of the dataset }
  \item{nosample}{ The size of the sample drawn and used to update the relevance of each feature. Usually it is equal to the total number of instances. }
  \item{threshold}{ The threshold for choosing the relevant features}
  \item{vnom}{ A vector of indices indicating the nominal features}
\item{repet}{The number of the repetitions. It is recommended to use at most 10 repetitions. If the nosample=number of instances then set repet=1}
}
\author{ Edgar Acuna}
\seealso{\code{\link{relief}}}
\keyword{Feature Selection}
