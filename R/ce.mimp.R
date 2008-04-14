`ce.mimp` <-
function (w.cl,method=c("mean","median"),atr,nomatr=0)
{
#find dimensions of matrix
p=dim(w.cl)

#find indexes of missing values
index.na=which(is.na(w.cl),arr.ind=TRUE)
o=order(index.na[,1], index.na[,2])
index.na=index.na[o, ]
dimnames(index.na)=NULL

#find variables with missing values
var.na=sort(as.numeric(names(table(index.na[,2]))))

#find values of var.na that are equal to relevant attributes, reduce var.na   
  var.na=var.na[var.na%in%atr]

if (length(var.na)==0) stop("Error: No missing values occur in relevant variables!")

#reduce rows of index.na to only (row,col) of relevant variables with missing 
  index.atr=matrix(index.na[index.na[,2] %in% atr[]],,2)

#find classes of rows with missing
  class.na=as.matrix(w.cl[index.atr[,1],p[2]])
  dimnames(class.na)=NULL
  class.na=cbind(index.atr,class.na)

  classes=sort(as.numeric(names(table(w.cl[index.na[,1],p[2]]))))
  num.class=length(classes)

  replace.na=rep(0,0)

#replace na is row with mean or median of class
  for(i in 1:dim(class.na)[1])
   {
    #split matrix into submatrices to find mean of class
     sub=w.cl[w.cl[,p[2]]==class.na[i,3],]
     #method=match.arg(method)

     if (class.na[i,2]%in%nomatr) imput.col=moda(sub[,class.na[i,2]])[1]
      else if (method=="mean") imput.col=mean(sub[,class.na[i,2]],na.rm=TRUE)
            else if (method=="median") imput.col=median(sub[,class.na[i,2]],na.rm=TRUE)
      
     #create a vector with imput value for column of class
     replace.na=rbind(replace.na,imput.col)      
   }
  dimnames(replace.na)=NULL
  class.na=cbind(class.na,replace.na)
  
  for (i in 1:dim(class.na)[1])  
    w.cl[class.na[i,1],class.na[i,2]]=class.na[i,4]
  

#Remove comments if screen view is desired
cat("\nSummary of imputations using substitution of ",method,"(mode for nominal features):\n")
colnames(class.na)=c("Row","Column","Class","Imput.value")
print(class.na)
cat("\nTotal number of imputations per class: \n")
for (i in classes)
  {
  amount=sum(class.na[,3]==i)
  cat("Class ",i,": ",amount,"\n")
 }
cat("\nTotal number of imputations: ",dim(class.na)[1],"\n")

#Remove comments if workspace result file is desired
#filename=paste("Imput.rep.",method,".",name,sep="")
#yy <- textConnection(filename, "w")
#rep.title=paste("Imputation report for the matrix: ",name)
#sink(yy)
#cat("\n",rep.title,"\n\n")
#cat("\nSummary of imputations using substitution of ",method,"(mode for nominal features):\n")
#colnames(class.na)=c("Row","Column","Class","Imput.value")
#print(class.na)
#cat("\nTotal number of imputations per class: \n")
#for (i in classes)
# {
#  amount=sum(class.na[,3]==i)
#  cat("Class ",i,": ",amount,"\n")
# }
#cat("\nTotal number of imputations: ",dim(class.na)[1],"\n")
#sink()
#close(yy)
#End comments to create workspace result file

return(w.cl)
}

