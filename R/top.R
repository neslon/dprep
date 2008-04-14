`top` <-
function(O,neighbors,n)
{

#Function that finds the n candidates for outliers that
#were requested by the user.

#O:  n x 1 matrix with the median distance from k nearest neighbors
#    of the n top outliers up to the moment.
#    row names are equal to names from original matrix D.

#neighbors: keeps distance from k nearest neighbors of prospective outliers
#           maximum size= blocksize x k, where k is number of nearest neighbors
#    

temp=as.matrix(apply(neighbors,1,sum))
#temp=as.matrix(apply(neighbors,1,median))
out=rbind(O,temp)
out.sort=as.matrix(out[order(out,decreasing=TRUE)])
outliers=as.matrix(out.sort[1:n,])
#outliers=as.matrix(out.sort)
return(outliers)
}

