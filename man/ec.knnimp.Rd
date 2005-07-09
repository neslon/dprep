\name{ec.knnimp}
\alias{ec.knnimp}
\title{KNN Imputation}
\description{
Function to carry out KNN imputation of missing values.
}
\usage{
ec.knnimp(data, nomatr, k = 10)
}
\arguments{
  \item{data}{ original dataset with missing values}
  \item{nomatr}{ vector containing the indexes of nominal attributes}
  \item{k}{ numeric value representing the number of neighbors to use for imputation.}
}
\details{
  This function is called by the function ce.knn.imp which is part of this library, to 
impute values by class. If called alone the function will impute values based on information
in the entire matrix and not the classes. Needs also the function: nnmiss.
}
\value{
  \item{data2}{ one class of a larger matrix for which missing values in relevant
variables have been imputed.}
}
\author{ Edgar Acuna and Caroline Rodriguez}
\references{Acuna, E. and Rodriguez, C. (2004). The treatment of missing values and its effect in the classifier accuracy. In D. Banks,  L. House, F.R. McMorris, P. Arabie, W. Gaul (Eds).
Classification, Clustering and Data Mining Applications. Springer-Verlag Berlin-Heidelberg, 639-648. 
}

\examples{
#---- Performing knn imputation----
data(hepatitis)
x.knnimp=ec.knnimp(hepatitis,k=10)
}
\keyword{manip}
