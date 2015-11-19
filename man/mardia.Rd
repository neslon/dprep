\name{mardia}
\alias{mardia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Mardia's test of normality}
\description{
Performs the Mardia's test to check for multivariate normality  
}
\usage{
mardia(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The dataset containing the features for which multivariate normality 
is going to be tested. The last column contains the class. In case of unsupervised data
add a dummy colummn of ones. In case of regression data, transform the response column in
a column of ones}
}
\value{
Returns the p-values for the corresponding third and fourth moments of the multivariate normal
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
data(iris)
mardia(iris)
}
\keyword{multivariate}
