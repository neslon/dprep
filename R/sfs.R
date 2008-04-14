`sfs` <-
function(data,method=c("lda","knn","rpart"),kvec=5,repet=10)
{
# *******************************************************
# This functions selects features using the sequential 
# forward method with lda, knn or rpart
#
# data: the data set
# method: choice of classifier
# kvec: the number of nearest neighbors to be used for the knn classifier
# rep: number of repetitions. rep=20 for small datasets and =10 for large datasets
# Required libraries: MASS, class and rpart
# Edgar Acuna- Caroline Rodriguez, March 2004
#---------------------------------------------------
require("MASS")
require("class")
require("rpart")
if (!(method %in% c("lda","knn","rpart")))
 {
  cat("The classifier entered is not supported by this function.\n")
  return(method)
 }
# n: number of instances
n=dim(data)[1]
# p: number of columns
p=dim(data)[2]
#Initializing the vector of the number of selected features in each repetition
numbersel=rep(0,repet)
#Initializing the frequencies of the features
fsel=rep(0,p-1)
for(i in 1:repet)
{# Initializing the vector that will contain the selected features
indic <- rep(0, p - 1)
output <- indic
#number of the column containing the classes
varia <- p
for(k in 1:(p-1)) {
correct <- rep(0, p - 1) #initializing the recognition rates for each feature
if(k > 1) {
varia <- c(where, varia)
}
for(m in 1:(p - 1)) {
if(indic[m] == 0) {
which <- c(m, varia)
   if (method=="lda") correct[m] <- cv10lda2(data[, which])
   else if (method=="knn") correct[m] <- cv10knn2(data[, which],kvec)
   else correct[m] <- cv10rpart2(data[, which])
}
}
prov <- correct + runif(p - 1) #Breaking ties randomly
where <- sum((1:(p - 1)) * as.numeric(max(prov) == prov))
#recognition rate of the entering feature
output[k] <- correct[where]/n
indic[where] <- 1
if(k > 1) {
if(output[k] <= output[k - 1]) {
#avoids ties of recognition rates
indic <- rep(1, p - 1)
}
}
}
which <- rev(which)
which <- which[-1]
which1 <- which[1:(length(which) - 1)]
#cat("Selected variables for ",method," classifier on this repetition are: \n")
#print(which1)
#list(varselec = which1, accuracy = output)
numbersel[i]=length(which1)
fsel[which1]=fsel[which1]+1
}
#print(numbersel)
#rounding the size of the best subset
bestsize=round(mean(numbersel))
#print(fsel)
rev(order(fsel))
bestsubset=rev(order(fsel))[1:bestsize]
cat("The best subset of features is:")
cat("\n")
bestsubset
}

