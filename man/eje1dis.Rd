\name{eje1dis}
\alias{eje1dis}
\docType{data}
\title{Basic example for discriminant analysis}
\description{
  This data frame contains information about 32 students. The first two columns contain 
their grades obtained in the first two exams and  the last column of the dataset contains 
the classes: P=Pass, and F=Fail 
}
\usage{data(eje1dis)}
\format{
  A data frame with 32 observations on the following 3 variables.
  \describe{
    \item{V1}{grade in the first exam}
    \item{V2}{grade in the second exam}
    \item{class}{the class vector:P=Pass, F=Fail}
  }
}
\source{ Data obtained from Edgar Acuna:
\itemize{
      \item \url{http://math.uprm.edu/~edgar/datosclass.html}
    }
}
\examples{
#---- Performing 10-fold cross validation using the LDA classifier-----
data(eje1dis)
crossval(eje1dis,10,"lda",repet=5)
}
\keyword{datasets}
