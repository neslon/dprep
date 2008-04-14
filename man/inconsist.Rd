\name{inconsist}
\alias{inconsist}
\title{Computing the inconsistency measure}
\description{
This function computes the inconsistency of a discretized dataset.
}
\usage{
inconsist(data)
}
\arguments{
  \item{data}{ a discretized dataset}
}
\details{
This function requires the function row.matches included in
this environment package, and the function unique from
the base library.}
\value{
  \item{incon}{ the inconsistency measure of the dataset}
}
\author{Edgar Acuna}
\references{
Dash M.,  Liu H, and  Motoda, H. (1998). Consistency Based Feature Selection  
Pacific-Asia Conference on Knowledge Discovery and Data Mining
}
\seealso{\code{\link{finco}}, \code{\link{lvf}}}
\examples{
##---- Calculating Inconsistency ----
data(bupa)
bupa.discew=disc.ew(bupa,1:6)
inconsist(bupa.discew)
}

\keyword{misc}
