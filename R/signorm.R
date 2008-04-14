`signorm` <-
function (data) 
{
#***********************************************
#This is a function to apply sigmoidal normalization to a matrix or dataframe.
#Sigmoidal normalization transforms the data into a range from [-1,1] by
#using a sigmoid function. 
#Input: data= The matrix or dataframe to be scaled
# Caroline Rodriguez (2004)
#********************************************************************

#store all attributes of the original data
d=dim(data)
c=class(data)
cnames=colnames(data)
classes=data[,d[2]]

#first step of sigmoidal normalization is to standardize data
zdata=znorm(data)

#remove classes from normalized dataset
d2=dim(zdata)
zdata=zdata[,-d2[2]]

#scaling used: (1-e^-zdata)/(1+e^-zdata)
sigdata=(1-exp(-zdata))/(1+exp(-zdata))

#return classes to normalized dataset
sigdata=cbind(sigdata,classes)

if (c=="data.frame") sigdata=as.data.frame(sigdata)
colnames(sigdata)=cnames
return(sigdata)

}

