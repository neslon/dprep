`sffs` <-
function(data,method=c("lda","knn","rpart"),kvec=5,repet=10)
{
# *******************************************************
# This function selects features using the sequential 
# floating forward method with lda, knn or rpart classifiers
#
# data: the data set
# method: choice of classifier
# kvec: the number of nearest neighbors to be used for the knn classifier
# Required libraries: MASS. class, and rpart
# Caroline Rodriguez-Edgar Acuna, March 2004
#------------------------------------------------
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
# p: number of variables
p=dim(data)[2]
grupos=data[,p]
# ngroups: number  of classes
ngroups=dim(table(data[,p]))
selected=rep(0,p)
numselect=0

for (j in 1:repet)
{
indic <- rep(0, p - 1)
correcto <- 0

paso1 <- sfs1(data,indic,correcto,kvec,method)
correcto <- paso1$accuracy
indic <- paso1$indic

i <- 2

while(i <= (p - 1)) 
{
  paso2 <- sfs1(data,indic,correcto,kvec,method)
  #print(paso2$accuracy)
  #print(correcto)
  if(paso2$accuracy > correcto) 
   {
    correcto <- paso2$accuracy
    indic <- paso2$indic
    for(j in 1:(i - 1)) 
     {
#cat("indic=",indic)
#cat("\n")
paso3 <- sbs1(data,indic,correcto,kvec,method)
correcto <- paso3$correcto
#print(correcto)
indic <- paso3$indic
     }
   }
  else 
   {
    i <- p
   }
}

variables <- seq(1, (p - 1))
variables <- variables[indic == 1]
#cat("Selected variables for ",method," classifier on this repetition are: \n")
#print(variables)
numselect=numselect+length(variables)
selected[variables]=selected[variables]+1
}
numselect=round(numselect/repet)
fselect=order(selected,decreasing=TRUE)[1:numselect]
cat("\nThe selected features are:\n")
return(fselect)
}

