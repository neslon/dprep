\name{censusn}
\alias{censusn}
\docType{data}
\title{The census dataset}
\description{
 This is the census dataset from the UCI where the values of the nominal attributes 
are numerically codified. This dataset contains plenty of missing values.  
}
\usage{data(censusn)}
\format{
  A data frame with 32561 observations on the following 14 variables.

  \describe{
    \item{V1}{age:continuous}
    \item{V2}{workclass:}
    \item{V3}{fnlwgt:continuous}
    \item{V4}{education}
    \item{V5}{marital-status:}
    \item{V6}{occupation:}
    \item{V7}{ relationship:}
    \item{V8}{race}
    \item{V9}{sex}
    \item{V10}{capital-gain: continuous.}
    \item{V11}{ capital-loss: continuous.}
    \item{V12}{ hours-per-week: continuous.}
    \item{V13}{native-country:}
    \item{V14}{ class: >50K, <=50K }
  }
}
\details{
  The fifth and fourth features of the orginal dataset were the same, since the  fifth contained the
numerical codifications of the fourth. In  censusn only one of these feature is considered.
The values of the nominal attributes are as follows: 
workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 
1st-4th, 10th, Doctorate, 5th-6th, Preschool.
marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, 
Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
sex: Female, Male.
native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, 
Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France,
 Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, 
El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands. 
}
\source{The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\examples{
data(censusn)
#----knn imputation------
data(censusn)
imagmiss(censusn, "censusn")
}
\keyword{datasets}

