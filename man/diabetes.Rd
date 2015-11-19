\name{diabetes}
\alias{diabetes}
\docType{data}
\title{The Pima Indian Diabetes dataset}
\description{
  This is the Pima Indian diabetes dataset from the UCI Machine Learning Repository.
}
\usage{data(diabetes)}
\format{
  A data frame with 768 observations on the following 9 variables.
  \describe{
    \item{V1}{Number of times pregnant}
    \item{V2}{Plasma glucose concentration (glucose tolerance test)}
    \item{V3}{Diastolic blood pressure (mm Hg)}
    \item{V4}{Triceps skin fold thickness (mm)}
    \item{V5}{2-Hour serum insulin (mu U/ml)}
    \item{V6}{Body mass index (weight in kg/(height in m)\^2)}
    \item{V7}{Diabetes pedigree function}
    \item{V8}{Age (years)}
    \item{V9}{Class variable (1:tested positive for diabetes, 0: tested negative fro diabetes)}
  }
}
\source{The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
#---Feature selection using SFS with the LDA classifier--
data(diabetes)
sfs(diabetes,"lda",repet=1)
}
\keyword{datasets}