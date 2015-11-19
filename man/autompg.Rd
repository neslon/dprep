\name{autompg}
\alias{autompg}
\docType{data}
\title{The Auto MPG dataset
}
\description{
This dataset was taken from the StatLib library which is maintained at Carnegie Mellon University. 
The dataset was used in the 1983 American Statistical Association Exposition. Six instances containing
missing values had been deleted.
}
\usage{data("autompg")}
\format{
  A data frame with 392 observations on the following 8 variables.
  \describe{
    \item{\code{mpg}}{a numeric vector indicating the mileage per gallon comsumption}
    \item{\code{cylinders}}{a numeric vector indicating the automobile's cylinders}
    \item{\code{displacement}}{a numeric vector}
    \item{\code{horsepower}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
    \item{\code{acceleration}}{a numeric vector}
    \item{\code{modelyear}}{a numeric vector indicating the automobile's year model}
    \item{\code{maker}}{a numeric vector. The value 1 is for american automobile, the value 2 is for european automobile, and
the value 3 is for an asian automobile}
}
  }
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
\dontrun{data(autompg)
maxlof(autompg,name="maxlof")
}}
\keyword{datasets}
