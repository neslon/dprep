\name{arboleje1}
\alias{arboleje1}
\docType{data}
\title{Predicting a bank's decision to give a loan for buying a car.
}
\description{
Simulated example about predicting a  bank's decision to give a loan to
customer for buying a car. The feature Marital Status is codified using
three dummy variables.} 
\usage{data("arboleje1")}
\format{
  A data frame with 25 observations on the following 9 variables.
  \describe{
    \item{\code{Sexo}}{a factor with levels \code{Hombre} \code{Mujer}}
    \item{\code{Familia}}{a numeric vector}
    \item{\code{CasaPropia}}{a factor with levels \code{No} \code{Si}}
    \item{\code{AnosEmpleo}}{a numeric vector}
    \item{\code{Sueldo}}{a numeric vector}
    \item{\code{Prestamo}}{a factor with levels \code{No} \code{Si}}
    \item{\code{x31}}{a numeric vector}
    \item{\code{x32}}{a numeric vector}
    \item{\code{x33}}{a numeric vector}
  }
}
\source{
Originated by Edgar Acuna
}
\examples{
data(arboleje1)
library(rpart)
rpart(Prestamo~.,data=arboleje1,method="class")
}
\keyword{datasets}
