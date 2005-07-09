"robout" <-
function(data,nclass,meth=c("mve","mcd"),rep=10,plot=TRUE)
{
#**********************************************************************
#This function finds out the outliers using robust versions of the
#Mahalanobis distance
#data: name of the dataset
#nclass: number of the class to check for outliers
#meth=method used to compute the Mahalanobis distance, "mve"=minimum
#      volume estimator, "mcd"=minimum covariance determinant. 
#rep= number of repetitions
#uses cov.rob function from the MASS library
#***********************************************************************
require(MASS)
ncol=dim(data)[2]
tempo=data[data[,ncol]==nclass,1:(ncol-1)]
namestempo=rownames(tempo)
nrow=dim(tempo)[1]
roboutl=NULL
roboutall=matrix(0,nrow,rep)
rownames(roboutall)=namestempo
for(i in 1:rep)
{mcdc=cov.rob(tempo,method=meth)
mbc=sqrt(mahalanobis(tempo,mcdc$center,mcdc$cov,to=.00000000000001))
roboutl=c(roboutl,boxplot(mbc,plot=FALSE)$out)
roboutall[,i]=mbc
}
a=as.matrix(roboutl)
b=apply(roboutall,1,mean)
outme=rev(sort(b))
topo=rev(sort(b))[1:10]
#print(topo)
if(plot){
#win.graph()
plot(rev(sort(b)),ylab=paste("Mahalabobis distance(",meth,")"))
text(1:10,topo,names(topo),cex=.6,pos=4)
}
top=table(as.numeric(rownames(a)))
#print(top)
top1=top[top>ceiling(rep/2)]
cat("\nTop outliers by frequency\n")
print(top1)
topout=as.numeric(names(top1))
ntops=length(topout)
outly=rep(0,ntops)
for(i in 1:ntops)
{outly[i]=mean(a[as.numeric(rownames(a))==topout[i]])
}
#topimp=cbind(topout,outly)
#topimp=cbind(topout,b[topout])
#topimp=topimp[order(-topimp[,2]),]
cat("\nTop outliers by outlyngness measure\n")
print(outme[1:ntops])
#zz=as.vector(outme)
#print(cbind(topout,zz[order(topout)]))
cat("\n")
list(outme=outme)
}

