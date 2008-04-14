\name{ce.knn.imp}
\alias{ce.knn.imp}
\title{Function that calls ec.knnimp to perform knn imputation}
\description{
This function simply sets up the dataset so that missing values 
can be imputed by knn imputation. ec.knnimp is the function that 
actually carries out the imputation.
}
\usage{
ce.knn.imp(m, natr = rep(0, 0), k1)
}
\arguments{
  \item{m}{ matrix containing relevant variables and classes
}
\item{natr}{ list of nominal attributes}
\item{k1}{ number of neighbors to use for imputation}
}
\value{
  \item{r }{ matrix with missing values imputed}
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
data(hepatitis)
hepa.knnimp=ce.knn.imp(hepatitis,natr=c(1,3:14),k1=10)
}

\keyword{ manip } 
