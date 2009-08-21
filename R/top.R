top <-
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
#cat("O\n")
#print(O)
#cat("neighbors\n")
#print(neighbors)
temp=as.matrix(apply(neighbors,1,sum))
#cat("sumas\n")
#print(temp)
#temp=as.matrix(apply(neighbors,1,median))
out=rbind(O,temp)
out=as.matrix(out)
#cat("out\n")
#print(out)
#cat("indices")
rowsn=as.numeric(rownames(out))
#print(rowsn)
#cat("indicesordenados\n")
rowsno=rowsn[order(out,decreasing=TRUE)]
#print(rowsno)
#out.sort=as.matrix(out[order(out,decreasing=TRUE)])
out.sort=as.matrix(out[order(-out)])
rownames(out.sort)=rowsno
#cat("out.sort\n")
#print(out.sort)
outliers=as.matrix(out.sort[1:n,])
#print(outliers)
#outliers=as.matrix(out.sort)
return(outliers)
}

