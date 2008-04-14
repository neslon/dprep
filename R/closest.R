`closest` <-
function(dis,neigh,k)
{
# Function used by baysout to select the k vectors that are closest to 
# a given observation  
# neigh:  matrix containing the distance to each of the k neighbors
#        rownames are rownames of D
# d:  is the instance from D under study, must have rowname
# k:  is number of nearest neighbors

new <- as.matrix(neigh)
maxindex <- which.max(new)
neigh[maxindex] <- dis

 return (neigh)
}

