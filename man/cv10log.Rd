\name{cv10log}
\alias{cv10log}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 10-fold cross validation estimation error for the classifier based on logistic regression  }
\description{
  10-fold cross validation estimation of the misclassification error for the classifier based on logistic regression 
}
\usage{
cv10log(data, repet,maxwts=2500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The name of the dataset}
  \item{repet}{ The number of repetitions}
  \item{maxwts}{ The maximum number of weights to be estimated. It must be an integer
                 number greater than the number of predictors of the dataset}
}
\value{
The mean cross validation error for the classifier based on logistic regression
using the number of repetition} 
\references{ 
Ripley, B.D. (1996). Pattern recognition and Neural networks. Cambridge University Press

Venables,W.N., and Ripley, B.D. (2002). Modern Applied Statistics with S. Fourth edition, Springer}
\author{Edgar Acuna}
\seealso{\code{\link{crossval}}, \code{\link{cv10mlp}} }
\examples{
#-----cross validation error for the logistic classifier-------
data(bupa)
cv10log(bupa,5)}
\keyword{classif}
