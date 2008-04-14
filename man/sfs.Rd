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
  \item{data}{ Dataset to be used for feature selection}
  \item{method}{ Classifier to be used, currently only the
lda, knn and rpart classifiers are supported }
  \item{kvec}{ Number of neighbors to use for the knn classification}
  \item{repet}{ Number of times to repeat the selection.}
}
\details{
  The best subset of features, T, is initialized as the empty set and at 
each step the feature that gives the highest correct classification
rate along with the features already in T, is added to set. 
The "best subset" of features is constructed based on the frequency 
with which each attribute is selected in the number of repetitions given.
Due to the time complexity of the algorithm its use is not recommended for
datasets with a large number of attributes(say more than 1000).
}
\value{
  \item{bestsubset}{ subset of features that have been determined to be
relevant. }
}
\author{Edgar Acuna }
\references{Acuna, E , (2003) A comparison of filters and wrappers for feature selection in supervised classification.
Proceedings of  the Interface 2003 Computing Science and Statistics. Vol 34.
}
\examples{
#---- Sequential forward selection using the knn classifier----
data(my.iris)
sfs(my.iris,method="knn",kvec=3,repet=10)
}
\keyword{methods}
