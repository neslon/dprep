"lofactor" <-
function(data,k)
{
# A function that finds the local outlier factor (Breunig,2000) of
# the matrix "data" with k neighbors
# Adapted by Caroline Rodriguez and Edgar Acuna, may 2004

data=as.matrix(data)

#find k nearest neighbors and their distance from each observation
#in data
 distdata=dist.to.knn(data,k)
 p=dim(distdata)[2]
 
#calculate the local reachability density for each observation in data
 lrddata=reachability(distdata,k)

 lof=rep(0,p)

#computer the local outlier factor of each observation in data
 for ( i in 1:p)
   {
nneigh=distdata[2,i]-distdata[1,i]+1
j=seq(0,(nneigh-1))
local.factor=sum(lrddata[distdata[3+j,i]]/lrddata[i])/nneigh
lof[i]=local.factor
   }

#return lof, a vector with the local outlier factor of each observation
 lof
}

