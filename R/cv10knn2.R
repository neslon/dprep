"cv10knn2" <-
function(data, kvec)
{
# This function finds out the number of instances correctly classified by
# the knn classifier, using 10-fold cross validation, with one repetition
# Requieres the knn function of the class library due to B. Ripley.
#inputs:
# data: dataset to be used
# k: number of nearest neighbors
#Edgar Acuna-Caroline Rodriguez, March 2004
#---------------------------------------------------------------------------------
n <- dim(data)[1]
p <- dim(data)[2]
salida <- matrix(0, 1, 10)
azar <- data[rank(runif(n)),  ]
azar[, p] <- as.factor(azar[, p])
parti <- floor(n/10)
for(j in 1:10) 
{
 cc <- ((j - 1) * parti + 1):(j * parti)
 if(j == 10) 
  {
   cc <- ((j - 1) * parti + 1):n
  }
 datap <- azar[cc,  ]
 datat <- azar[ - cc,  ]
 tempo <- knn(as.matrix(datat[, 1:p - 1]), as.matrix(datap[, 1:p - 1]), datat[, p], kvec)
 salida[j] <- sum(tempo != as.numeric(datap[, p]))
}
#print(salida)
ECV1 <- n-sum(salida)
#cat(" The number of instances correctly classified is:\n")
#print(ECV1)
return(ECV1)
}

