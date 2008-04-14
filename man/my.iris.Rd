\name{my.iris}
\alias{my.iris}
\docType{data}
\title{The Iris dataset}
\description{
This is perhaps the best known database to be found in the pattern recognition literature. 
Fisher's paper is a classic in the field and is referenced frequently to this day. 
(See Duda & Hart, for example.) The data set contains 3 classes of 50 instances each, 
where each class refers to a type of iris plant. The Setosa class is linearly separable from 
the other two classes. The last two classes are NOT linearly separable from each other. 
}
\usage{data(my.iris)}
\format{
  A dataframe with 150 observations on the following 5 variables. 
  \describe{
    \item{V1}{sepal length in cm}
    \item{V2}{sepal width in cm }
    \item{V3}{petal length in cm }
    \item{V4}{petal width in cm}
    \item{V5}{class: Iris Setosa(1), Iris Versicolor(2),Iris Virginica(3)}
  }
}
\source{
The UCI Machine Learning Database Repository at:
\itemize{
      \item \url{ftp://ftp.ics.uci.edu/pub/machine-learning-databases}
      \item \url{http://www.ics.uci.edu/~mlearn/MLRepository.html}
    }
}
\references{
Fisher, R. A. (1936) The use of multiple measurements in taxonomic
     problems. Annals of Eugenics, Vol 7, Part II, 179--188.
}
\examples{
#----Testing multivariate normality----
data(my.iris)
mardia(my.iris)
}
\keyword{datasets}

