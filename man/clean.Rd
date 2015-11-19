\name{clean}
\alias{clean}
\title{Dataset's cleaning}
\description{
A function to eliminate rows and columns that have 
a percentage of missing values greater than the allowed 
tolerance.
}
\usage{
clean(w, tol.col = 0.5, tol.row = 0.3, name = "")
}
\arguments{
  \item{w}{ the dataset to be examined and cleaned}
  \item{tol.col}{ maximum ratio of missing values allowed in columns. The
default value is 0.5. Columns with a larger ratio of missing will be eliminated
unless they are known to be relevant attributes. 
}
  \item{tol.row}{ maximum ratio of missing values allowed in rows. The
default value is 0.3. Rows with a ratio of missing that is larger that the
established tolerance will be eliminated.
}
  \item{name}{ name of the dataset to be used for the optional report}
}
\details{
 This function can create an optional report on the cleaning
process if the comment symbols are removed from the last lines of code.
The report is returned to the workspace, where it can be reexamined
as needed. The report object's name begins with: Clean.rep.
}
\value{
  \item{w}{ the original dataset, with missing values that were in
relevant variables imputed}
}
\author{Caroline Rodriguez}
\references{Acuna, E. and Rodriguez, C. (2004). The treatment of missing values and its effect in the classifier accuracy.
In D. Banks,  L. House, F.R. McMorris, P. Arabie, W. Gaul (Eds).
Classification, Clustering and Data Mining Applications. Springer-Verlag Berlin-Heidelberg, 639-648. 
}
\seealso{\code{\link{ce.impute}}}
\examples{
#-----Dataset cleaning-----
data(hepatitis)
hepa.cl=clean(hepatitis,0.5,0.3,name="hepatitis-clean")
}
\keyword{Imputation}
