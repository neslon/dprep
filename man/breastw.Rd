\name{breastw}
\alias{breastw}
\docType{data}
\title{ The Breast Wisconsin dataset}
\description{
  This is the Breast Wisconsin dataset from the UCI Machine Learning Repository. 
Sixteen instances with missing values have been deleted from the original dataset.
}
\usage{data(breastw)}
\format{
  A data frame with 683 observations on the following 10 variables.
  \describe{
    \item{V1}{Clump Thickness }
    \item{V2}{Uniformity of Cell Size}
    \item{V3}{Uniformity of Cell Shape}
    \item{V4}{Marginal Adhesion}
    \item{V5}{Single Epithelial Cell Size}
    \item{V6}{Bare Nuclei}
    \item{V7}{Bland Chromatin }
    \item{V8}{Normal Nucleoli}
    \item{V9}{Mitoses}
    \item{V10}{Class: 1 for benign, 2 for Malign}
  }
}
\details{
All the features assume values in the range 1-10. The original dataset
contains 699 observations but 16 of them have been delate because contain
missing values
}
\source{The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
#Detecting outliers in class-1 using the LOF algorithms---
data(breastw)
maxlof(breastw[breastw[,10]==1,],name="",30,40)
}
\keyword{datasets}
