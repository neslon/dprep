\name{outbox}
\alias{outbox}
\title{ Detecting outliers through boxplots of the features. }
\description{
This function detects univariate outliers simultaneously using boxplots of the features.
}
\usage{
outbox(data, nclass)
}
\arguments{
  \item{data}{ the dataset to be explored for outlier detection.}
  \item{nclass}{ a value representing the number that will be explored.}
}
\details{
  The function also displays a plot containing a boxplot for
of the variables.
}
\value{
  \item{out1}{ list of the indices of the observations that are outside the extremes
of the boxplot. The indices are given in a table format representing the number of columns 
in which the observation was identified as an outlier.}
  }
\author{Edgar Acuna}
\examples{
#---- Identifying outliers in diabetes-class1 with boxplots----
data(diabetes)
outbox(diabetes,nclass=1)
}
\keyword{methods}
