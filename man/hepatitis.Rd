\name{hepatitis}
\alias{hepatitis}
\docType{data}
\title{The hepatitis dataset}
\description{
  This is the hepatitis dataset from the UCI. The data was donated by Gail Gong.
}
\usage{data(hepatitis)}
\format{
  A data frame with 155 observations on the following 20 variables.
This dataset contains a large number of missing values.
  \describe{
    \item{V1}{Histology:no,yes}
    \item{V2}{age}
    \item{V3}{sex: male,female}
    \item{V4}{steroid:no,yes}
    \item{V5}{antivirals:no,yes}
    \item{V6}{fatigue:no, yes}
    \item{V7}{malaise:no, yes}
    \item{V8}{anorexia:no, yes}
    \item{V9}{liver big:no,yes}
    \item{V10}{liver firm:no,yes}
    \item{V11}{spleen palpable: no, yes}
    \item{V12}{spiders:no,yes}
    \item{V13}{ascites:no,yes}
    \item{V14}{Varices:no,yes}
    \item{V15}{Bilirubin}
    \item{V16}{alk phosphate}
    \item{V17}{sgot}
    \item{V18}{Albumin}
    \item{V19}{Protime}
    \item{V20}{Class:Die, Live}
  }
}
\details{
The original dataset has the classes labels in the first column.
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\references{
Diaconis,P. & Efron,B. (1983). Computer-Intensive Methods in Statistics. 
Scientific American, Volume 248. }
\examples{
#------Report and plot of missing values ------
data(hepatitis)
imagmiss(hepatitis,"hepatitis")
}
\keyword{datasets}
