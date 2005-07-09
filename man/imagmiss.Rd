\name{imagmiss}
\alias{imagmiss}
\title{Visualization of Missing Data}
\description{
  Function to create a graph of the observations of the dataset
leaving white lines where data is missing.
} 
\usage{
imagmiss(data, name = "")
}
\arguments{
  \item{data}{ dataset containing missing values}
  \item{name}{ name of dataset to be used in title of plot}
}
\details{
  The main idea is to use the original dataset to create a temporary 
dataset containing 1 if a value is found or 0 is the value is missing. 
The temporary data set is graphed by column changing color for each 
feature and leaving a blank horizontal line if a value is missing. Assumes 
classes are in the last column, and removes the column containing the classes
before plotting.
}
\author{Caroline Rodriguez and Edgar Acuna }
\references{Acuna, E. and Rodriguez, C. (2004). The treatment of missing values and its effect in the classifier accuracy. In D. Banks,  L. House, F.R. McMorris, P. Arabie, W. Gaul (Eds).
Classification, Clustering and Data Mining Applications. Springer-Verlag Berlin-Heidelberg, 639-648. 
}

\examples{
#---- Plotting datasets with missing values
data(censusn)
imagmiss(censusn, "censusn")
}
\keyword{hplot}
