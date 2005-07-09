"mahaout" <-
function(data,nclass,plot=TRUE)
{
#**********************************************************************
#This function finds out the outliers through
#the boxplot of the all Mahalanobis distance 
#data: name of the dataset
#nclass: number of the class to check for outliers
#uses cov.rob function from the MASS library
#***********************************************************************
require(MASS)
ncol=dim(data)[2]
tempo=data[data[,ncol]==nclass,1:(ncol-1)]
namestempo=rownames(tempo)
nrow=dim(tempo)[1]
roboutl=NULL
mcdc=cov.rob(tempo,method="classical")
mbc=sqrt(mahalanobis(tempo,mcdc$center,mcdc$cov,to=.00000000000001))
roboutl=c(roboutl,boxplot(mbc,plot=FALSE)$out)
cat("Ouliers given by the boxplot of the  Mahalanobis distance\n")
print(rev(sort(roboutl)))
outme=rev(sort(mbc))
topo=rev(sort(mbc))[1:10]
if(plot){
#win.graph()
plot(rev(sort(mbc)),ylab="Mahalabobis distance")
text(1:10,topo,names(topo),cex=.6,pos=4)
}
cat("\n")
list(outme=outme)
}

