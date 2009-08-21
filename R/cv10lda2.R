cv10lda2 <-
function(data)
{
# This function finds the number of instances correctly classified 
# by the Linear Discriminant classifier using 10 fold cross validation 
# with one repetition
# Inputs:
# data: dataset including the classes in the last colummn
# Requires the lda function of the MASS library due to Ripley.
# Edgar Acuna-Caroline Rodriguez, 2004
#-----------------------------------------------------
n<-dim(data)[1]
p<-dim(data)[2] 

salida <- matrix(0, 1, 10)
azar <- data[rank(runif(n)),  ]
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
 tempo <- lda(as.matrix(datat[, 1:p - 1]), datat[, p])
 tempo1 <- predict(tempo, as.matrix(datap[, 1:p - 1]))$class
 salida[j] <- sum(tempo1 != as.numeric(datap[, p]))
}

gooderr <- n-sum(salida)

return(gooderr)
}

