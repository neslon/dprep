maxdist <-
function(dneighbors)
{
#Function used by baysout to find the largest value of a distance vector
#returns the value and the index number
#dneighbors: row vector with the distance of the k nearest neighbors for a given b of B

dneighbors=as.matrix(dneighbors)
maxindex=which.max(dneighbors)
max=dneighbors[maxindex]
list(value=max,index=maxindex)
}

