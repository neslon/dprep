`ce.impute` <-
function(data,method=c("mean","median","knn"),atr,nomatr=rep(0,0),k1=10)
{
if (method=="mean")
data.impute=ce.mimp(data,method,atr,nomatr)
else if (method=="median")
  data.impute=ce.mimp(data,method,atr,nomatr)
else data.impute=ec.knnimp(data,nomatr,k1)
}

