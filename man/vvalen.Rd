\name{vvalen}
\alias{vvalen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Van Valen test for equal covariance matrices }
\description{
  The Van Valen nonparametric test for homocedasticity (equal covariance matrices).
}
\usage{
vvalen(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The name of the dataset to be tested }
}
\value{
Gives the p-value for a Kruskal Wallis test. A  p-value near to zero
indicates homocedasticity.
}
\references{Van Valen, L. (1962). A study of fluctuating asymmetry. Evolution Vol. 16, pp. 125142. }
\author{Edgar Acuna}
\seealso{\code{\link{mardia}}}
\examples{
#---------Testing homocedasticity-----
data(colon)
vvalen(colon)
}
\keyword{misc}
