\name{colon}
\alias{colon}
\docType{data}
\title{The Alon et al.'s colon dataset}
\description{
This is the Alon et al.'s Colon cancer dataset which contains information of 
62 samples on 2000 genes. The samples belong to tumor and normal colon tissues.
}
\usage{data(colon)}
\format{
  A data frame with 62 observations on 2000 genes. An additional column contains the 
tissues classes.
}
\source{ The data is available at:
\itemize{
      \item \url{http://microarray.princeton.edu/oncology/}
}
}
\references{
Alon U, Barkai N, Notterman DA, Gish, K, Ybarra, S. Mack, D and Levine, AJ. (1999). Broad patterns of 
gene expression revealed by clustering analysis of tumor and normal colon tissues probed by oligonucleotide
 arrays. Proc. Natl. Acad. Sci. USA. 96. p. 6745-6750. 
}
\examples{
#Detecting the top 5 outliers in class-2 using the LOF algorithm
data(colon)
colon2.lof=maxlof(colon[colon[,2001]==2,],"colon-class2")
colon2.lof[order(colon2.lof,decreasing=TRUE)][1:5]
}
\keyword{datasets}
