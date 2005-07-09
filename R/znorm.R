"znorm" <-
function (data) 
{
#This is a function to apply z-Score normalization to a matrix or datafram.
#Uses scale function found in the R base package.
#Input: data= The matrix or dataframe to be scaled

#store all attributes of the original data
d=dim(data)
c=class(data)
cnames=colnames(data)

#remove classes from dataset
classes=data[,d[2]]
data=data[,-d[2]]

zdata=scale(data)

#remove attributes added by the function scale and turn resulting
#vector back into a matrix with original dimensions
attributes(zdata)=NULL
zdata=matrix(zdata,dim(data)[1],dim(data)[2])
zdata=cbind(zdata,classes)

if (c=="data.frame") zdata=as.data.frame(zdata)
colnames(zdata)=cnames
return(zdata)

}

