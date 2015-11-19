\name{Shuttle}
\alias{Shuttle}
\docType{data}
\title{
The Shuttle dataset
}
\description{
This is the Shuttle dataset from the Stalog project.
}
\usage{data("Shuttle")}
\format{
  A data frame with 58000 instances and 10 variables.
The shuttle dataset contains 9 attributes all of which are numerical.
The last column is the class which has 7 values,
        1       Rad Flow
        2       Fpv Close
        3       Fpv Open
        4       High
        5       Bypass
        6       Bpv Close
        7       Bpv Open
         }
\details{
Approximately 80% of the data belongs to class 1
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
\dontrun{data(Shuttle)
mmnorm(Shuttle)
}}
\keyword{datasets}
