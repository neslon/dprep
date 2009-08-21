baysout <-
function(D,blocks=5,k=3,num.out=10)
{
#Function that gives the outlyingness measure for the requested number of
#observations using the algorithms developed by Bay.
#D: the dataset
#blocks: size of block to be processed
#k: number of nearest neighbors to compute to determine if observation
#  is a candidate for an outlier
#num.out: number of candidates for outliers and their outlyingness measure
#  to display as output. Must be less than or equal to block number.
big = 1000000
D=as.data.frame(D)
nrows=dim(D)[1]
#DB = data.frame(D[i:f,])
#rownames(DB)=rownames(D[i:f,])
brows = dim(D)[1]
#cat("\n B size = ",brows)
c=0
Out=NULL
#rep=ceiling(brows/blocks)
rep=ceiling(nrows/blocks)
for (cycle in 1:rep){ #for (cycle in 1:1){
  block.size=blocks
  if (block.size*cycle<=brows) block=(block.size*(cycle-1)+1):(block.size*cycle) 
  else {block=(block.size*(cycle-1)+1):brows;block.size=length(block)}
  #B=DB[block,]
  B = D[block,]
  neighbors=matrix(rep(big,(block.size*k)),block.size,k)
  rownames(neighbors)=rownames(B)
  neighbors=as.data.frame(neighbors)
  for (m in 1:nrows){ # for (m in i:f)
     #cat("\n data-iter \t",m)
     d=D[m,]   
     j=1
     flag=0
     reduce = 0
     removeB=rep(0,0)
     removeN=rep(0,0) 
    # print(neighbors)
     while ((j <= block.size)&(block.size>=1)){
       if (!(as.integer(rownames(D)[m])==as.integer(rownames(B)[j]))){
b=B[j,]
dis = distancia(b,d)
        #if ((0%in%neighbors[j,])|(distancia(b,d)<maxdist(neighbors[j,])[1])){
if (dis < maxdist(neighbors[j,])[1]){
           neighbors[j,]=closest(dis,neighbors[j,],3)
   if (score(neighbors[j,]) < c){
               removeB=cbind(removeB,j)
               removeN=cbind(removeN,j)
               reduce=reduce + 1
              }
          }
        }
        j=j+1
      }
#      print(neighbors)

      if (reduce==dim(B)[1]) flag=1
      else if(reduce != 0){
          block.size=block.size-reduce
    B=B[-removeB,]
          neighbors=neighbors[-removeN,]
      }
   }
   if (flag==0)
   {
    Out=top(Out,neighbors,num.out)
#print(Out)
    c=min(Out)
#print(c)
   }
}
#print(Out)
xcoord=as.integer(rownames(Out))
plot(Out,main="Instances with Greatest score from K nearest neighbors",ylab="Median Distance")
text(1:num.out,Out,rownames(Out),cex=.6,pos=4)
#print(Out)
return(Out)
}

