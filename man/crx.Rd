\name{crx}
\alias{crx}
\docType{data}
\title{
crx
}
\description{
The Australian credit Aproval dataset form the Statlog Project
}
\usage{data("crx")}
\format{
  A data frame with 690 observations on the following 16 variables.
  \describe{
    \item{\code{V1}}{a factor with levels \code{a} \code{b}}
    \item{\code{V2}}{a numeric vector}
    \item{\code{V3}}{a numeric vector}
    \item{\code{V4}}{a factor with levels \code{l} \code{u} \code{y}}
    \item{\code{V5}}{a factor with levels \code{g} \code{gg} \code{p}}
    \item{\code{V6}}{a factor with levels \code{aa} \code{c} \code{cc} \code{d} \code{e} \code{ff} \code{i} \code{j} \code{k} \code{m} \code{q} \code{r} \code{w} \code{x}}
    \item{\code{V7}}{a factor with levels \code{bb} \code{dd} \code{ff} \code{h} \code{j} \code{n} \code{o} \code{v} \code{z}}
    \item{\code{V8}}{a numeric vector}
    \item{\code{V9}}{a factor with levels \code{f} \code{t}}
    \item{\code{V10}}{a factor with levels \code{f} \code{t}}
    \item{\code{V11}}{a numeric vector}
    \item{\code{V12}}{a factor with levels \code{f} \code{t}}
    \item{\code{V13}}{a factor with levels \code{g} \code{p} \code{s}}
    \item{\code{V14}}{a numeric vector}
    \item{\code{V15}}{a numeric vector}
    \item{\code{V16}}{a numeric vector}
  }
}
\details{
This dataset contains missing values.
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
data(crx)
ec.knnimp(crx,nomatr=c(1,4:7,9,10,12,13),k=3)
}
\keyword{datasets}
