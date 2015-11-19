\name{imagmiss}
\alias{imagmiss}
\title{Visualization of Missing Data}
\description{
  Function to create a graph of the observations of the dataset
leaving white gaps where data is missing.
} 
\usage{
imagmiss(data, name = "")
}
\arguments{
  \item{data}{ The dataset containing missing values}
  \item{name}{ The name of dataset to be used in title of plot}
}
\details{
  The main idea is to use the original dataset to create a temporary 
dataset containing 1 if a value is found or 0 if the value is missing. 
The temporary data set is graphed by column, changing color for each 
feature and leaving a blank horizontal line if a value is missing. Assumes 
classes are in the last column, and removes the column containing the classes
before plotting. A report that describes the percentage of missing values in the data set
is provided once the visualization is complete. 
}
\author{Caroline Rodriguez and Edgar Acuna }
\references{Acuna, E. and Rodriguez, C. (2004). The treatment of missing values and its effect in the classifier accuracy. In D. Banks,  L. House, F.R. McMorris, P. Arabie, W. Gaul (Eds).
Classification, Clustering and Data Mining Applications. Springer-Verlag Berlin-Heidelberg, 639-648. 
}

\examples{
#---- Plotting datasets with missing values
data(hepatitis)
imagmiss(hepatitis, "hepatitis")
}
\keyword{Visualization}

