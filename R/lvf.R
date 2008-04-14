`lvf` <-
function(data, lambda, maxiter)
{
#*****************************************
#This function performs the LVF algorithm
#Data:name of the discretized dataset
#Lambda: Threshold for the inconsistency
#maxiter:Maximun number of iterations
# It requires the function inconsist
# Edgar Acuna, May 2004
#******************************************
f <- dim(data)[2] - 1
bestsize <- f
variables <- 1:f
bestsubset <- 1:f
count <- 0
bestinselec=lambda
for(i in 1:maxiter) 
{
repeat {
indic <- rbinom(f, 1, 0.5)
if(sum(indic) > 0)
break
}
subset <- variables[indic > 0]
#size of the best subset
csubset <- length(subset)
#print(subset)
if(csubset <= bestsize) 
{
inselec <- inconsist(data[, c(subset, f + 1)])
#print(inselec)
if(inselec <bestinselec) 
{
if(csubset<bestsize)
{bestsubset <- subset
bestsize <- csubset
#bestinselec <- inselec
#cat(subset,"\n")
}
#else 
#{cat(subset,"\n")}
}
}
}

cat("The inconsistency of the best subset is\n")
cat(inconsist(data[,c(bestsubset,f+1)]))
cat("\nThe best subset of features is:\n")
bestsubset
}

