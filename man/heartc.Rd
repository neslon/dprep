\name{heartc}
\alias{heartc}
\docType{data}
\title{The Heart Cleveland dataset}
\description{
  This dataset contains information concerning heart disease diagnosis. 
The data was collected from the  Cleveland Clinic Foundation, and it is available 
at the UCI machine learning Repository. Six instances containing missing values.
}
\usage{data(heartc)}
\format{
  A data frame with 297 observations on the following 14 variables.
  \describe{
    \item{V1}{age(continuous)}
    \item{V2}{sex}
    \item{V3}{cp, chest pain type:1,2,3,4}
    \item{V4}{trestbps: resting blood pressure(continuous)}
    \item{V5}{cholesterol(continuous)}
    \item{V6}{fps: fatsing blood sugar>120? yes=1, no =0}
    \item{V7}{restecg: resting electrocardiographic results, 0,1, 2}
    \item{V8}{thalach: maximum heart rate achieved(continuous) }
    \item{V9}{exang: exercise induced angina (1 = yes; 0 = no)}
    \item{V10}{oldpeak = ST depression induced by exercise relative to rest (continuous)}
    \item{V11}{slope: the slope of the peak exercise ST segment}
    \item{V12}{ca: number of major vessels (0-3) colored by flourosopy}
    \item{V13}{thal: 3 = normal; 6 = fixed defect; 7 = reversable defect}
    \item{V14}{diagnosis of heart disease: 1: < 50% diameter narrowing,
              2: > 50% diameter narrowing}
  }
}
}
\details{
This dataset contains six instances having missing values. It is recommended to impute
these values before applying other tasks. This dataset includes continuous, binomial, nominal, 
and ordinal features.}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}

\examples{
#----Detecting outliers using the Relief---
data(heartc)
imagmiss(heartc,"heart-Cleveland")
}
\keyword{datasets}
