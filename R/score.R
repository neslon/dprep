"score" <-
function(data)
{
#Function to determine the score measure that will be used to determine
#candidates for outliers
#data: vector containing distances to k nearest neighbors.

#s=median(as.matrix(data))
s=sum(as.matrix(data))
return (s)
}

