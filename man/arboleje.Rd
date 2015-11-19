\name{arboleje}
\alias{arboleje}
\docType{data}
\title{Predicting a bank's decision to give a loan for buying a car.
}
\description{
Simulated example about predicting a  bank's decision to give a loan to
customer for buying a car.} 
\usage{data("arboleje")}
\format{
  A data frame with 25 observations on the following 7 variables.
  \describe{
    \item{\code{Sexo}}{a factor indicaing the customer's gender with levels \code{Hombre} \code{Mujer}}
    \item{\code{Familia}}{a numeric vector indicating the number of members in the family}
    \item{\code{CasaPropia}}{a factor with levels \code{No} \code{Si}}
    \item{\code{AnosEmpleo}}{a numeric vector indicating the years of employment}
    \item{\code{Sueldo}}{a numeric vector indicating the monthly salary}
    \item{\code{StatusMarital}}{a factor with levels \code{Casado} \code{Divorciado} \code{Soltero} \code{Viudo}}
    \item{\code{Prestamo}}{a factor indicating the bank's with levels \code{No} \code{Si}}
  }
}
\source{
Originated by EDgar Acuna
}
\examples{
data(arboleje)
library(rpart)
rpart(Prestamo~.,data=arboleje,method="class")
}
\keyword{datasets}
