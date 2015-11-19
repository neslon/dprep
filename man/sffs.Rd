\name{sffs}
\alias{sffs}
\title{Sequential Floating Forward Method}
\description{
  This function selects features using the sequential 
floating forward method with lda, knn or rpart classifiers.
}
\usage{
sffs(data, method = c("lda", "knn", "rpart"), kvec = 5, 
repet = 10)
}
\arguments{
  \item{data}{ Dataset to be used for feature selection}
  \item{method}{ String sequence representing the choice of classifier}
  \item{kvec}{ The number of nearest neighbors to be used for the knn classifier}
  \item{repet}{ Integer value representing the number of repetitions}
}
\details{
  The Sequential Floating Forward selection method was introduced to deal with the nesting problem. 
The best subset of features, T, is initialized as the empty set and at each step a new feature is added. 
After that, the algorithm searches for features that can be removed from T until the
correct classification error does not increase. This algorithm is a combination
of the sequential forward and the sequential backward methods.
The "best subset" of features is constructed based on the frequency 
with which each attribute is selected in the number of repetitions given.
Due to the time complexity of the algorithm its use is not recommended for
data sets with a a large number of attributes(say more than 1000).
}
\value{
  \item{fselect}{ a list of the indices of the best features}
}
\references{
Pudil, P., Ferri, J., Novovicova, J., and Kittler, J. (1994). 
Floating search methods for feature selection with nonmonotonic criterion 
function. 12 International Conference on Pattern Recognition, 279-283. 

Acuna, E , (2003) A comparison of filters and wrappers for feature selection in supervised classification.
Proceedings of  the Interface 2003 Computing Science and Statistics. Vol 34.
}
\author{Edgar Acuna}
\examples{
#---- SFFS feature selection using the knn classifier ----
data(iris)
sffs(iris,method="rpart",repet=2)
}
\keyword{Feature Selection}
