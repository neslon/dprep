"decscale" <-
function (data) 
{
#This is a function to apply decimal scaling to a matrix or dataframe.
#Decimal scaling transforms the data into a range from [-1,1] by
#finding k such that the absolute value of the maximum value of each attribute divided by 10^k  
#is less than or equal to 1.
#Uses the scale function found in the R base package.
#Input: data= The matrix or dataframe to be scaled

#store all attributes of the original data
d=dim(data)
c=class(data)
cnames=colnames(data)

#remove classes from dataset
classes=data[,d[2]]
data=data[,-d[2]]

maxvect=apply(abs(data),2,max)

#find k such that max/10^k is less than 1.
kvector=ceiling(log10(maxvect))
scalefactor=10^kvector
decdata=scale(data,center=FALSE,scale=scalefactor)

#remove attributes added by the function scale and turn resulting
#vector back into a matrix with original dimensions
attributes(decdata)=NULL
decdata=matrix(decdata,dim(data)[1],dim(data)[2])
decdata=cbind(decdata,classes)

if (c=="data.frame") decdata=as.data.frame(decdata)
colnames(decdata)=cnames
return(decdata)

}

