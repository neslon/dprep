\name{vehicle}
\alias{vehicle}
\docType{data}
\title{ The Vehicle dataset}
\description{
  This is the vehicle dataset from the UCI Machine Learning Repository 
}
\usage{data(vehicle)}
\format{
  A data frame with 846 observations on the following 19 variables.
  \describe{     
    \item{V1}{Compactness}
    \item{V2}{Circularity}
    \item{V3}{Distance Circularity}
    \item{V4}{Radius ratio}
    \item{V5}{pr.axis aspect ratio}
    \item{V6}{max.length aspect ratio}
    \item{V7}{scatter ratio}
    \item{V8}{elongatedness}
    \item{V9}{pr.axis rectangularity}
    \item{V10}{max.length rectangularity}
    \item{V11}{scaled variance along major axis}
    \item{V12}{scaled variance along minor axis}
    \item{V13}{scaled radius of gyration}
    \item{V14}{skewness about major axis}
    \item{V15}{skewness about minor axis}
    \item{V16}{kurtosis about minor axis}
    \item{V17}{kurtosis about major axis}
    \item{V18}{hollows ratio}
    \item{V19}{Type of vehicle: a double decker bus, Cheverolet van, Saab 9000 and an Opel Manta
     400.}
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
#----feature selection using sequential floating selection with LDA----
data(vehicle)
mahaout(vehicle,nclass=3)
}
\keyword{datasets}
