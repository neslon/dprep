\name{ce.impute}
\alias{ce.impute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Imputation in supervised classification}
\description{
  This function performs data imputation in datasets for supervised classification by using
mean, median or knn imputation methods. The mode is used when the attribute is nominal
}
\usage{
ce.impute(data, method = c("mean", "median", "knn"), atr,
 nomatr = rep(0, 0), k1 = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the name of the dataset }
  \item{method}{ the name of the method to be used }
  \item{atr}{ a vector identifying the attributes where imputations will be performed }
  \item{nomatr}{ a vector identifying the nominal attributes}
  \item{k1}{ the number of neighbors to be used for the knn imputation}
}
\value{
Returns a matrix without missing values.
}
\references{Acuna, E. and Rodriguez, C. (2004). The treatment of missing values and its effect in the classifier accuracy. In D. Banks,  L. House, F.R. McMorris, P. Arabie, W. Gaul (Eds).
Classification, Clustering and Data Mining Applications. Springer-Verlag Berlin-Heidelberg, 639-648. 
}
\author{Caroline Rodriguez}
\note{A description of all the imputations carried out may be stored in a report
that is later saved to the current workspace. To produce the report, lines
at the end of the code must be uncommented. The report objects name starts
with Imput.rep.
}
\seealso{\code{\link{clean}}}
\examples{
data(hepatitis)
#--------Median Imputation-----------
#ce.impute(hepatitis,"median",1:19)
#--------knn Imputation--------------
hepa.imputed=ce.impute(hepatitis,"knn",k1=10)
}

\keyword{ Imputation }% 
