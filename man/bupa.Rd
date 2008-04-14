\name{bupa}
\alias{bupa}
\docType{data}
\title{The Bupa dataset}
\description{
  The Bupa dataset
}
\usage{data(bupa)}
\format{
  A data frame with 345 observations on the following 7 variables.
  \describe{
    \item{V1}{mean corpuscular volume}
    \item{V2}{alkaline phosphotase}
    \item{V3}{alamine aminotransferase}
    \item{V4}{aspartate aminotransferase}
    \item{V5}{gamma-glutamyl transpeptidase}
    \item{V6}{number of half-pint equivalents of alcoholic beverages drunk per day }
    \item{V7}{The class variable (two classes)}
  }
}
\source{The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
#---Sequential forward feature selection using the lda classifier---
data(bupa)
sfs(bupa,"lda",repet=10)
}
\keyword{datasets}
