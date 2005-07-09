\name{decscale}
\alias{decscale}
\title{ Decimal Scaling }
\description{
  This is a function to apply decimal scaling to a matrix or dataframe.
Decimal scaling transforms the data into [-1,1] by finding k such that 
the absolute value of the maximum value of each attribute divided by 10\^k  
is less than or equal to 1.
}
\usage{
decscale(data)
}
\arguments{
  \item{data}{ dataset to be scaled}
}
\details{
  Uses the scale function found in the R base package.
}
\value{
  \item{decdata }{original matrix that has been scaled by decimal scaling}
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
data(sonar)
def=par(mfrow=c(2,1))
plot(sonar[,2])
dssonar=decscale(sonar)
plot(dssonar[,2])
par(def)
}
\keyword{manip}
