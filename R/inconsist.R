inconsist <-
function(data)
{
#*******************************************************************
# This fucntion computes the inconsistency of a discretized  dataset
# data:name of the discretized dataset
# It requires the function row.matches
# Edgar Acuna, February 2004
#**********************************************************************
data=as.matrix(data)
dimnames(data)=NULL
n=dim(data)[1]
p=dim(data)[2]
if(p==2)
{cardi=table(data[,1])
#print(cardi)
unicos=unique(data[,1])
#print(unicos)
mfrecu=max(table(data[data[,1]==unicos[1],2]))
for(j in unicos[-1])
{#cat(j,"\t")
tempo=max(table(data[data[,1]==j,2]))
mfrecu=c(mfrecu,tempo)
}
#cat("\n")
#print(mfrecu)
mfrecu=mfrecu[mfrecu>0]
}
else
{
datau=unique(data[,1:(p-1)])
rowsu=dim(datau)[1]
#print(datau[1,1:p-1])
tempo=row.matches(datau[1,1:(p-1)],data[,1:(p-1)])
ind=tempo
#print(ind)
mfrecu=max(table(data[ind,p]))
cardi=length(ind)
#print(mfrecu)
#print(cardi)
datatempo=as.matrix(data[-ind,])
#print(datatempo)
for(j in 2:(rowsu-1))
{tempov=datau[j,1:p-1]
#cat("\n",j,"\t",tempov)
tempo=row.matches(tempov,datatempo[,1:p-1])
#cat("\n",j,"\t",tempo)
ind=tempo
#cat("frequency maxima")
mfrecu=c(mfrecu,max(table(datatempo[ind,p])))
#print(mfrecu)
datatempo=datatempo[-ind,]
#print(dim(datatempo))
cardi=c(cardi,length(ind))
}
#print(dim(datatempo))
if(length(dim(datatempo))==0)
{cardi=c(cardi,dim(t(as.matrix(datatempo)))[1])
 mfrecu=c(mfrecu,1)}
else
{cardi=c(cardi,dim(datatempo)[1])
#cat("\ntabla2\n")
#print(datatempo)
#print(ind)
#print(table(datatempo[,p]))
mfrecu=c(mfrecu,max(table(datatempo[,p])))
#cat(mfrecu)
}
}
#print(mfrecu)
#cat("\n")
#cat(length(cardi))
#cat("\n")
incon=sum(cardi-mfrecu)/n
incon
}

