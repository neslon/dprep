\name{census}
\alias{census}
\docType{data}
\title{
census
}
\description{
This is the census (also known as adult) dataset. It is used to predict the salary of a person
based on socio-demographis and economic predictors.
}
\usage{data("census")}
\format{
  A data frame with 32561 observations on the following 15 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{employment}}{a factor with levels \code{ Federal-gov} \code{ Local-gov} \code{ Never-worked} \code{ Private} \code{ Self-emp-inc} \code{ Self-emp-not-inc} \code{ State-gov} \code{ Without-pay}}
    \item{\code{a3}}{a numeric vector}
    \item{\code{education}}{a factor with levels \code{ 10th} \code{ 11th} \code{ 12th} \code{ 1st-4th} \code{ 5th-6th} \code{ 7th-8th} \code{ 9th} \code{ Assoc-acdm} \code{ Assoc-voc} \code{ Bachelors} \code{ Doctorate} \code{ HS-grad} \code{ Masters} \code{ Preschool} \code{ Prof-school} \code{ Some-college}}
    \item{\code{education.num}}{a numeric vector}
    \item{\code{marital.status}}{a factor with levels \code{ Divorced} \code{ Married-AF-spouse} \code{ Married-civ-spouse} \code{ Married-spouse-absent} \code{ Never-married} \code{ Separated} \code{ Widowed}}
    \item{\code{job}}{a factor with levels \code{ Adm-clerical} \code{ Armed-Forces} \code{ Craft-repair} \code{ Exec-managerial} \code{ Farming-fishing} \code{ Handlers-cleaners} \code{ Machine-op-inspct} \code{ Other-service} \code{ Priv-house-serv} \code{ Prof-specialty} \code{ Protective-serv} \code{ Sales} \code{ Tech-support} \code{ Transport-moving}}
    \item{\code{relationship}}{a factor with levels \code{ Husband} \code{ Not-in-family} \code{ Other-relative} \code{ Own-child} \code{ Unmarried} \code{ Wife}}
    \item{\code{race}}{a factor with levels \code{ Amer-Indian-Eskimo} \code{ Asian-Pac-Islander} \code{ Black} \code{ Other} \code{ White}}
    \item{\code{gender}}{a factor with levels \code{ Female} \code{ Male}}
    \item{\code{a11}}{a numeric vector}
    \item{\code{a12}}{a numeric vector}
    \item{\code{hours.per.week}}{a numeric vector}
    \item{\code{native.country}}{a factor with levels \code{ Cambodia} \code{ Canada} \code{ China} \code{ Columbia} \code{ Cuba} \code{ Dominican-Republic} \code{ Ecuador} \code{ El-Salvador} \code{ England} \code{ France} \code{ Germany} \code{ Greece} \code{ Guatemala} \code{ Haiti} \code{ Holand-Netherlands} \code{ Honduras} \code{ Hong} \code{ Hungary} \code{ India} \code{ Iran} \code{ Ireland} \code{ Italy} \code{ Jamaica} \code{ Japan} \code{ Laos} \code{ Mexico} \code{ Nicaragua} \code{ Outlying-US(Guam-USVI-etc)} \code{ Peru} \code{ Philippines} \code{ Poland} \code{ Portugal} \code{ Puerto-Rico} \code{ Scotland} \code{ South} \code{ Taiwan} \code{ Thailand} \code{ Trinadad&Tobago} \code{ United-States} \code{ Vietnam} \code{ Yugoslavia}}
    \item{\code{salary}}{a factor with levels \code{ <=50K} \code{ >50K}}
  }
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
\dontrun{data(census)
imagmiss(census,"census")
}}
\keyword{datasets}
