\name{ionosphere}
\alias{ionosphere}
\docType{data}
\title{The Ionosphere dataset}
\description{
  The Ionosphere dataset from the UCI Machine Learning Repository
}
\usage{data(ionosphere)}
\format{
  A data frame with 351 observations on the following 33 variables.
}
\details{
The original dataset contains 34 predictors, but we have eliminated the two first features,
because the first feature had the same value in one of the classes and the second feature assumes
the value 0 in all observations.
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
#---Outlier detection in  ionosphere class-1 using the Mahalanobis distiance----
data(ionosphere)
mahaout(ionosphere,1)
}
\keyword{datasets}
