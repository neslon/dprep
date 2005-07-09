\name{cv10lda2}
\alias{cv10lda2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary function for sequential forward selection}
\description{
  This function finds the number of instances correctly classified 
by the Linear Discriminant classifier using 10 fold cross validation 
with one repetition.
}
\usage{
cv10lda2(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{the name of the dataset}
}
\author{Edgar Acuna}
\seealso{\code{\link{crossval}}}
\keyword{classif}% at least one, from doc/KEYWORDS
