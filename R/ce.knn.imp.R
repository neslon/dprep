ce.knn.imp <-
function(m,natr=rep(0,0),k1)
{
#A function that sends a matrix by class to have missing
#values imputed using knn imputation
#Inputs:
#  m      : matrix to be tested with relevant variables and classes
#  natr: list of nominal attributes
#  k1: number of neighbors to use.
#Output : complete matrix with missing values imputed

m=as.matrix(m)

dr=dim(m)[1]
dc=dim(m)[2]

classes=tabulate(m[,dc])
no.classes=length(classes)

r=NULL

for(i in 1:no.classes) 
{
m.imp=ec.knnimp(m[m[,dc]==i,],natr,k1)
r=rbind(r,m.imp)
}

return(r)
}

