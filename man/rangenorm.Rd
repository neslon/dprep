\name{rangenorm}
\alias{rangenorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{range normalization }
\description{
 Performs several methods of range normalization. 
}
\usage{
rangenorm(data, method = c("znorm", "mmnorm", "decscale","signorm", "softmaxnorm"),superv=TRUE)
}
\arguments{
  \item{data}{The name of the dataset to be normalized}
  \item{method}{The discretization method to be used:"znorm", "mmnrom", "decscale", "signorm", "softmaxnorm"}
  \item{superv} {superv=T for supervised data, that data including the class labels in the last column. 
if superv=F means that the data to be used is unsupervised.}
}
\details{
  In the znorm normalization, the mean of each attribute of the transformed set
of data points is reduced to zero by subtracting the mean of each
attribute from the values of the attributes and dividing the difference 
by the standard deviation of the attribute. Uses the function scale found in the base library.

  Min-max normalization (mmnorm) subtracts the minimum value of an attribute from each value
of the attribute and then divides the difference by the range of the attribute.
These new values are multiplied by the new range of the attribute
and finally added to the new minimum value of the attribute.
These operations transform the data into a new range, generally [0,1].

 The decscale normalization applies decimal scaling to a matrix or dataframe.
Decimal scaling transforms the data into [-1,1] by finding k such that 
the absolute value of the maximum value of each attribute divided by 10\^k  
is less than or equal to 1.

   In the sigmoidal normalization (signorm) the input data is nonlinearly 
transformed into [-1,1] using a sigmoid function. The original data is first 
centered about the mean, and then mapped to the almost linear region of the sigmoid.
Is especially appropriate when outlying values are present.

  The softmax normalization is so called because it reaches "softly" towards
maximum and minimum value, never quite getting there. The
transformation is more or less linear in the middle range, and has a
nonlinearity at both ends. The output range covered is [0,1].  The
algorithm removes the classes of the dataset before normalization
and replaces them at the end to form the matrix again.

}
\value{
A matriz containing the discretized data.
}
\references{ 
Caroline Rodriguez (2004). An computational environmnent for data preprocessing 
in supervised classification. Master thesis. UPR-Mayaguez

Hann, J., Kamber, M. (2000). Data Mining: Concepts and Techniques. 
 Morgan Kaufman Publishers.
}
\author{Caroline Rodriguez and Edgar Acuna}
\examples{
#----Several methods of range normalization ----
data(bupa)
bupa.znorm=rangenorm(bupa,method="znorm")
bupa.mmnorm=rangenorm(bupa,method="mmnorm")
bupa.decs=rangenorm(bupa,method="decscale")
bupa.signorm=rangenorm(bupa,method="signorm")
bupa.soft=rangenorm(bupa,method="softmaxnorm")
#----Plotting to see the effect of the normalization----
op=par(mfrow=c(2,3))
plot(bupa[,1])
plot(bupa.znorm[,1])
plot(bupa.mmnorm[,1])
plot(bupa.decs[,1])
plot(bupa.signorm[,1])
plot(bupa.soft[,1])
par(op)
}
\keyword{manip}
