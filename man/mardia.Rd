\name{mardia}
\alias{mardia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Mardia's test of normality}
\description{
Performs the Mardia'st test in order to check multivariate normality  
}
\usage{
mardia(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The dataset containing the features which multivariate normality 
is going to be tested}
%  \item{superv}{If True performs Mardia test in each class. If false performs Mardia test
fo the whole data.}
}
\value{
Returns the p-values for the corresponding third and four moments of the multivarute normal
distribution. 
}
\references{
Mardia, K.V. (1985). "Mardia's Test of Multinormality," in S. Kotz
and N.L. Johnson, eds., Encyclopedia of Statistical Sciences,
vol. 5 (NY: Wiley), pp. 217-221. }
\author{ Edgar Acuna}
\seealso{\code{\link{vvalen}}}
\examples{
#-----Mardia test for supervised data-----
data(my.iris)
mardia(my.iris)
#----Mardia test for unsupervised data-----
#data(hawkins)
#mardia(hawkins,superv=F)
}
\keyword{multivariate}
