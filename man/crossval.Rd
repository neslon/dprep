\name{crossval}
\alias{crossval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Cross validation estimation of the misclassification error}
\description{
  Computation of the misclassification error for the  LDA, KNN and rpart classifiers by cross validation}
\usage{
crossval(data, nparts = 10, method = c("lda", "knn", "rpart","logistic","naiveBayes"), 
kvec = 5,maxwts=2500, repet)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The name of the dataset}
  \item{nparts}{ The number of folds in which the dataset is divided. By default nparts=10.}
  \item{method}{ The name of the classifier to be used: LDA,KNN, Rpart.}
  \item{kvec}{ The number of nearest neighbors to be used for the KNN classifier.}
\item{maxwts}{The maximum number of iteratios to be used in th computation of the logistic regression}
  \item{repet}{ The number of repetitions}
}
\value{
Returns the mean misclassification crossvalidation error of the classifier obtained on
a given number of repetitions}
\author{Edgar Acuna}

\seealso{ \code{\link{cv10log}}, \code{\link{cv10mlp}}}
\examples{
#------10-fold crossvalidation error using the LDA classifier---
data(bupa)
crossval(bupa,method="lda",repet=10)
\dontrun{#------5-fold crossvalidation error using the knn classifier---
data(colon)
crossval(colon,nparts=5,method="knn",kvec=3,repet=5)
}
}
\keyword{classification}
