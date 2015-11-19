\name{sonar}
\alias{sonar}
\docType{data}
\title{The Sonar dataset}
\description{
This is the sonar dataset. It containis information on 208 objects and 60 attributes.
The objects are classified in two classes: "rock" and "mine".
}
\usage{data(sonar)}
\format{
  A data frame with 208 observations on 61 variables. The first 60
represent the energy within a particular frequency band,integrated over a 
certain period of time.  The last column contains the class labels. There are two 
classes 0 if the object is a rock, and 1 if the object is a mine (metal cylinder).
The range value of each attribute varies from 0.0 to 1.0.
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
\dontrun{#Robust detection of outliers in sonar-class1 using MVE----
data(sonar)
robout(sonar,1,"mve",rep=10)
}
}
\keyword{datasets}
