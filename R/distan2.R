distan2 <-
function(x,y,vnom){
indices=1:length(x)
if(length(vnom)==length(x))
{tempo1=0
tempo2=sum(x!=y)
}
else
{ind1=indices[-vnom]
#print(ind1)
tempo1=sum((x[ind1]-y[ind1])*(x[ind1]-y[ind1]))
#print(tempo1)
tempo2=sum(x[-ind1]!=y[-ind1])
#print(tempo2)
}
dist=tempo1+tempo2
dist
}

