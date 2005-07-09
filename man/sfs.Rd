\name{sfs}
\alias{sfs}
\title{Sequential Forward Selection}
\description{
  Applies the Sequential Forward Selection algorithm for 
Feature Selection.
}
\usage{
sfs(data, method = c("lda", "knn", "rpart"), kvec = 5,
 repet = 10)
}
\arguments{
  \item{data}{ dataset to be used for feature selection}
  \item{method}{ classifier to be used, currently only the
lda, knn and rpart classifiers are supported }
  \item{kvec}{ number of neighbors to use for the knn classification}
  \item{repet}{ number of time to repeat the selection.}
}
\details{
  The best subset of features is initialized as the empty set and at 
each step a the feature that gives the highest correct classification
rate along with the features already included is added to set. 
The "best subset" of features is constructed based on the frequency 
with which each attribute is selected in the number of repetitions given.
Due to the time complexity of the algorithm its use is not recommended for
a large number of attributes(say more than 1000).
}
\value{
  \item{bestsubset}{ subset of features that have been determined to be
relevant. }
}
\author{Edgar Acuna }
\references{Acuña, E , (2003) A comparison of filters and wrappers for feature selection in supervised classification.
Proceedings of  the Interface 2003 Computing Science and Statistics. Vol 34.
}
\examples{
#---- Sequential forward selection using the knn classifier----
data(my.iris)
sfs(my.iris,method="knn",kvec=3,repet=10)
}
\keyword{methods}
