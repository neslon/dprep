"vvalen" <-
function (data) 
{#***************************************
#This function performs the Van Valen test
#for equality of covariance matrices (homocedasticity
#******************************************
p=dim(data)[2]
f=p-1
clases=length(table(data[,p]))
testlist=vvalen1(data,1)
testlist=list(testlist)
for(i in 2:clases)
{a=vvalen1(data,i)
testlist=c(testlist,list(a))
}
z=kruskal.test(testlist)
return(z)
}

