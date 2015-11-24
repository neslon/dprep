\name{cvnaiveBayesd}
\alias{cvnaiveBayesd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crossvalidation estimation error for the naive Bayes classifier.
}
\description{
This function computes the crossvalidaton error for a naive bayes classifier after 
discretization
}
\usage{
cvnaiveBayesd(data, repet, method = c("ew", "ef", "1R", "chiMerge"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The dataset
}
  \item{repet}{
The number of repetions.
}
  \item{method}{
The discretezation method to be used, bins with equal widths (ew), bins with equal frequency (ef),
Holte's oneR (1R) and chiMerge.
}
}
\details{
Uses 10-fold crossvalidation.
}
\value{Returns the mean misclassification crossvalidation error of the classifier obtained on
a given number of repetions}
\author{
Edgar Acuna
}

\seealso{\code{\link{crossval}}
}
\examples{
data(diabetes)
library(e1071)
cvnaiveBayesd(diabetes,3,method="ew")
}
\keyword{classification }

