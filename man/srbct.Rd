\name{srbct}
\alias{srbct}
\docType{data}
\title{The Khan et al.'s small round blood cells dataset}
\description{
The sbrct dataset which contains information of 63 samples and 2308 genes. The samples
are distributed in four classes as follows: 8 Burkitt Lymphoma (BL), 23 Ewing Sarcoma
 (EWS), 12 neuroblastoma (NB), and 20 rhabdomyosarcoma (RMS). 
 The last column contains the labels of the fourth classes
}
\usage{data(srbct)}
\format{
 A data frame containing 63 observations of 2308. The last column of the dat frame 
contains the class labels for each observations. 
}
\source{
The data set was  obtained, as binary R file from Marcel Dettling's web site:
\itemize{
      \item \url{http://stat.ethz.ch/~dettling/bagboost.html}
    }
}
\references{
Javed Khan, Jun S. Wei, Markus Ringner, Lao H. Saal, Marc Ladanyi, Frank Westermann,
Frank Berthold, Manfred Schwab, Cristina R. Antonescu, Carsten Peterson, and 
Paul S. Meltzer (2001). Classification and diagnostic prediction of cancers using gene
expression profiling and artificial neural networks. Nature Medicine, Volume 7, Number 6, June 
}
\examples{
#---z-score Normalization
data(srbct)
rangenorm(srbct,"znorm")
#---feature selection using the RELIEF-----
#relief(srbct,63,0.12)
}
\keyword{datasets}
