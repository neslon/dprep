\name{ce.mimp}
\alias{ce.mimp}
\title{Mean or median imputation}
\description{  A function that detects the location of missing values
by class, then imputes the missing values that occur in the features,
using mean or median imputation, as selected by the user. If the feature
is nominal then imputation is done using the mode.
}
\usage{
ce.mimp(w.cl, method = c("mean", "median"), atr, nomatr = 0)
}
\arguments{
  \item{w.cl}{ dataset with missing values.}
  \item{method}{ either "mean" or "median"}
  \item{atr}{ list of relevant features}
  \item{nomatr}{ list of nominal features, imputation is done using mode}
}
\value{
  \item{w.cl }{the original matrix with values imputed}
}
\author{Caroline Rodriguez and Edgar Acuna}
\note{A description of all the imputations carried out may be stored in a report
that is later saved to the current workspace. To produce the report, lines
at the end of the code must be uncommented. The report objects name starts
with Imput.rep.
}
\references{Acuna, E. and Rodriguez, C. (2004). The treatment of missing values and its effect in the classifier accuracy. In D. Banks,  L. House, F.R. McMorris, P. Arabie, W. Gaul (Eds).
Classification, Clustering and Data Mining Applications. Springer-Verlag Berlin-Heidelberg, 639-648. 
}
\examples{
data(hepatitis)
#--------Mean Imputation---------- 
hepa.mean.imp=ce.impute(hepatitis,"mean",1:19)
}
\keyword{ Imputation}
