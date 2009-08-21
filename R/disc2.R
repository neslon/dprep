disc2 <-
function (x,k) 
{
#****************************************
# Auxiliar fucntion for disc.ef
# Edgar Acuna (2005)
#**************************************
n=length(x)
#print(n)
ciclo=ceiling(n/k)
#print(ciclo)
y=x
for(i in 1:(k-1))
{y[order(x)[((i-1)*ciclo+1):(i*ciclo)]]=i}
y[order(x)[((k-1)*ciclo+1):n]]=k
#print(x)
return(y)
}

