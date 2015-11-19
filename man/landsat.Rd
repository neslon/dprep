\name{landsat}
\alias{landsat}
\docType{data}
\title{
The landsat Satellite dataset
}
\description{
This is the Landsat Satellite dataset from the Stalog project.
The training and test dataset have been joined to form a single dataset
}
\usage{data("landsat")}
\format{
  A data frame with 6435 observations and 37 variables.
 }
\details{
The training set has 4435 intances y el test set kas 2000 instances.
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
\dontrun{data(landsat)
relief(landsat,200,0,3)
}
}
\keyword{datasets}
