disc.ef <-
function(data, varcon,k)
{
#****************************************************
#This function discretizes the columns of a dataset 
#using the equal frequency method
#data:name of the dataset to be discretized
#varcon: vector containing the columns to be discretized.
#k: number of intervals per column
# Edgar Acuna (2005)
#********************************************************
data=as.matrix(data)
p <- dim(data)[2]
f <- p - 1
ft <- rep(0, f)
for(i in 1:length(varcon)) {
ft[varcon[i]]=1
}
for(i in 1:f) {
if(ft[i] > 0) {
data[, i] <- disc2(as.vector(data[,i]),k)
}
}
data
}

