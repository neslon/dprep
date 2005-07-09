"softmaxnorm" <-
function (data) 
{
#This is a function to apply softmax normalization to a matrix or dataframe.
#Softmax normalization transforms the data into a range from [0,1] by
#Input: data= The matrix or dataframe to be scaled


#store all attributes of the original data
d=dim(data)
c=class(data)
cnames=colnames(data)
classes=data[,d[2]]

#first step of softmax normalization is to standardize data
zdata=znorm(data)


#remove classes from standardized dataset
d2=dim(zdata)
zdata=zdata[,-d2[2]]

#scaling used: 1/(1+e^-zdata)
softdata=1/(1+exp(-zdata))

softdata=cbind(softdata,classes)

if (c=="data.frame") softdata=as.data.frame(softdata)
colnames(softdata)=cnames
return(softdata)

}

