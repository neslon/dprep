\name{ec.knnimp}
\alias{ec.knnimp}
\title{Imputation using k-nearest neighbors.}
\description{
This function imputes missing values by knn imputation.
}
\usage{
ec.knnimp(data, nomatr=0, k=10)
}
\arguments{
  \item{data}{ matrix containing relevant variables and classes
}
\item{nomatr}{ list of nominal attributes}
\item{k}{ number of neighbors to use for imputation}
}
\value{
  \item{r }{ matrix with missing values imputed}
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
data(hepatitis)
hepa.knnimp=ec.knnimp(hepatitis,nomatr=c(1,3:14),k=10)
}
\keyword{ Imputation} 
