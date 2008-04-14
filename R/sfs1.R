`sfs1` <-
function(data,indic,correcto,kvec,method=c("lda","knn","rpart"))
{
# This function carries out one "forward step" using 
# lda, knn o rpart classifiers.
# imputs:
# data: the dataset
# indic: vector of 0's and 1's. 1 indicates the variable in that position
# has been selected and 0 that it has not been selected.
# correcto=recognition rate of the current best subset
# kvec : the number of nearest neighbors
# Edgar Acuna-Caroline Rodriguez, March 2004
#------------------------------------------------------------------
# n: number of instances
n=dim(data)[1]
# p: number of variables
p=dim(data)[2]
output<-indic
varia <- 1:(p - 1)
varia <- varia[indic > 0] 
#print(varia)

#Initializing the recognition rate vector
correct <- rep(0, p - 1)

for(m in 1:(p - 1)) 
{
 if(indic[m] == 0) 
  {
   which <- c(m, varia, p) 
   if (method=="lda") correct[m] <- cv10lda2(data[, which])
   else if (method=="knn") correct[m] <- cv10knn2(data[, which],kvec)
   else correct[m] <- cv10rpart2(data[, which])
  }
}

#print(correct)

#Breaking ties randomly
prov <- correct + runif(p - 1) 
#print(prov)

#The entering feature
where <- which(max(prov) == prov)
#print(where)

#recognition rate of the entering feature
output <- correct[where]/n  
#print(output)

if(output > correcto) 
{
indic[where] <- 1
}

list(indic = indic, varselec = where, accuracy = output)
}

