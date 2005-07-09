\name{cv10mlp}
\alias{cv10mlp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 10-fold cross validation error estimation for the multilayer perceptron classifier}
\description{
10-fold cross validation estimation error for the multilayer perceptron classifier.
}
\usage{
cv10mlp(data, units, decay = 0, maxwts = 1000, maxit = 100, 
repet)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The name of the dataset}
  \item{units}{ The number of units in the hidden layer }
  \item{decay}{ The decay parameter}
  \item{maxwts}{ The maximum number of weights to be estimated in the network}
  \item{maxit}{ The maximum number of iterations}
  \item{repet}{ The number of repetitions}
}
\value{
Returns the mean cross validation for the multilayer perceptron classifier}
\references{
Ripley, B.D. (1996). Pattern recognition and Neural networks. Cambridge University Press

Venables,W.N., and Ripley, B.D. (2002). Modern Applied Statistics with S. Fourth edition, Springer}}
\author{ Edgar Acuna}
\seealso{\code{\link{crossval}}, \code{\link{cv10log}} }
\examples{
#-----cross validation using the MLP classifier---
data(heartc)
cv10mlp(heartc,25,decay=0.1,maxwts=1000,maxit=100,repet=5)
}
\keyword{classif}
