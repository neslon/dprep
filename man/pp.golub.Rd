\name{pp.golub}
\alias{pp.golub}
\docType{data}
\title{The preprocessed Golub's dataset}
\description{
This is the preprocessed Golub's dataset, where the training and the learning
set are first joined and then preprocessed according to the Dudoit et al.'s paper.}
\usage{data(pp.golub)}
\format{
  A data frame with 72 observations on the following 3572 variables.
}
\source{
This data set is available at:
\itemize{
      \item \url{http://www-genome.wi.mit.edu/cgi-bin/cancer/datasets.cgi}
      \item \url{http://www.bioconductor.org}
    }
}
\references{
Dudoit, S., Fridlyand, J. & Speed, T. P. (2002) Comparison of discrimination methods for the classi¯cation of tumors suing gene
expression data. J Am Stat Assoc, 97 (457), 77--87.

Golub, T. R., Slonim, D. K., Tamayo, P., Huard, C., Gaasenbeek, M., Mesirov, J. P., Coller, H., Loh, M. L., Downing, J. R.,
Caligiuri, M. A., Bloomfield, C. D. & Lander, E. S. (1999) Molecular classification of cancer: class discovery and class prediction
by gene expression monitoring. Science, 286, 531--537.
}
\examples{
#----z-score Normalization---
data(pp.golub)
rangenorm(pp.golub,"znorm")
}
\keyword{datasets}
