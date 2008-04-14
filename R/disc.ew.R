`disc.ew` <-
function(data, varcon)
{
#****************************************************
#This function discretizes the columns of a dataset
#using one equal width method 
#data:name of the dataset to be discretized
#varcon: vector containing the columns to be discretized.
#Edgar Acuna (2004)
#********************************************************
p <- dim(data)[2]
f <- p - 1
ft <- rep(0, f)
for(i in 1:length(varcon)) {
ft[varcon[i]] <- 1
}
for(i in 1:f) {
if(ft[i] > 0) {
grupos <- nclass.scott(data[, i])
data[, i] <- as.vector(cut(data[, i], grupos,labels=FALSE))
}
}
data
}

