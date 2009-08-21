knneigh.vect <-
function(x,data,k)
{
#Function that returns the distance from a vector "x" to   
#its k-nearest-neighbors in the matrix "data"

temp=as.matrix(data)
numrow=dim(data)[1]
dimnames(temp)=NULL

#subtract rowvector x from each row of data
difference<- scale(temp, x, FALSE)

#square and add all differences and then take the square root
dtemp <- drop(difference^2 %*% rep(1, ncol(data)))
dtemp=sqrt(dtemp)

#order the distances
order.dist <- order(dtemp)
nndist=dtemp[order.dist]

#find distance to k-nearest neighbor
#uses k+1 since first distance in vector is a 0
knndist=nndist[k+1]

#find neighborhood
#eliminate first row of zeros from neighborhood 
neighborhood=drop(nndist[nndist<=knndist])
neighborhood=neighborhood[-1]
numneigh=length(neighborhood)

#find indexes of each neighbor in the neighborhood
index.neigh=order.dist[1:numneigh+1]

# this will become the index of the distance to first neighbor
num1=length(index.neigh)+3

# this will become the index of the distance to last neighbor
num2=length(index.neigh)+numneigh+2

#form a vector
neigh.dist=c(num1,num2,index.neigh,neighborhood)

return(neigh.dist)
}

