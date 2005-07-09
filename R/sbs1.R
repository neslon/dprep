"sbs1" <-
function(data,indic,correct0,kvec,method=c("lda","knn","rpart"))
{
# This function performs a step of the Backward selection method using the classifiers
# LDA, knn, or rpart
# data: dataset to be used
# indic: vector of 0's and 1's. 1 indicates the variable in that position
# has been removed and 0 that it has not been removed.
# correct0;recognition rate of the current best subset
# Edgar Acuna-Caroline Rodriguez, March 2004
# --------------------------------------------------
# n: number of instances
n=dim(data)[1]
# p: number of features
p=dim(data)[2]
output <- indic
varia <- 1:(p - 1)
varia <- varia[indic > 0]
#print(varia)

#initializing the recognition rate vector
correct <- rep(0, p - 1)
 
mm=0
for(m in 1:(p - 1)) 
 {
  if(indic[m] == 1) 
  {
    mm=mm+1
    #print(mm)
    #print(varia)
    temp<-varia
     #print(temp)
    which <- temp[ - mm]
     #print(which)
    if (method=="lda") correct[m] <- cv10lda2(data[, c(which, p)])
    else if (method=="knn") correct[m] <- cv10knn2(data[, c(which, p)],kvec)
    else correct[m] <- cv10rpart2(data[, c(which, p)])
  }
 }

#Breaking ties randomly
prov <- correct + runif(p - 1)
 
#The feature to be removed
where <- sum((1:(p - 1)) * as.numeric(max(prov) == prov))

#recognition rate of the removed feature
output <- correct[where]/n  

if(output >= correct0) 
{
  indic[where] <- 1
}
else 
{
  output <- correct0
  where <- NULL
  which1 <- NULL
}

which <- rev(which)

which1 <- where
indic[where] <- 0 

list(variaelim = which1, indic = indic, correcto = output)
}

