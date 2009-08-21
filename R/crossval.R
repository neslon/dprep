crossval <-
function(data,nparts=10,method=c("lda","knn","rpart"),kvec=5,repet)
{
# This function finds an estimate for the
# misclasification error through 10-cross validation
# for Fisher Linear Classifier
# Requires the lda function from the "MASS" library.
# inputs:
#  data: the dataset to be used with classes in the last column
#  k: the number of nearest neighbors
#  repet: the number of repetitions
#  Edgar Acuna, 2004
#-----------------------------------------------------
if(method=="lda")
{require(MASS)
n<-dim(data)[1]
p<-dim(data)[2] 
errorcv<-rep(0,repet)
for(i in 1:repet)
{
salida <- matrix(0, 1, nparts)
azar <- data[rank(runif(n)),  ]
#azar[, p] <- as.factor(azar[, p])
parti <- floor(n/nparts)
for(j in 1:nparts) {
cc <- ((j - 1) * parti + 1):(j * parti)
if(j == nparts) {
cc <- ((j - 1) * parti + 1):n
}
datap <- azar[cc,  ]
datat <- azar[ - cc,  ]
tempo <- lda(as.matrix(datat[, 1:p - 1]), datat[, p])
tempo1 <- predict(tempo, as.matrix(datap[, 1:p -1]))$class
salida[j] <- sum(as.numeric(tempo1) != as.numeric(datap[, p]))
}
errorcv[i] <- sum(salida)/n
}
#cat("errores en cada repeticion",errorcv,"\n")
#cat("error promedio",mean(errorcv),"\n")
errorp=mean(errorcv)
}
if(method=="rpart")
{require(rpart)
 datos=as.data.frame(data)
 n <- dim(datos)[1]
 p <- dim(datos)[2]
ecv<-rep(0,repet)
#print(ecv)
for(kk in 1:repet)
{
nombres<-colnames(datos)
 f1<-as.formula(paste(nombres[p],".",sep="~"))
 salida <- matrix(0, 1, nparts)
 azar <- datos[rank(runif(n)),  ]
 azar[, p] <- as.factor(azar[, p])
 parti <- floor(n/nparts)
 for(j in 1:nparts) 
  {
   cc <- ((j - 1) * parti + 1):(j * parti)
   if(j == nparts) 
   {
    cc <- ((j - 1) * parti + 1):n
   }
   datap <- azar[cc,  ]
   datat <- azar[ - cc,  ]
   arbol <- rpart(f1, data = datat, method="class")
   pd1<-predict(arbol,datap)
   pd2=max.col(pd1)
   salida[j] <- sum(pd2!=as.numeric(datap[, p]))
  }
ecv[kk] <- sum(salida)/n
}
errorp=mean(ecv)
}
if(method=="knn"){
require(class)
n <- dim(data)[1]
p <- dim(data)[2]
ecv<-rep(0,repet)

for(kk in 1:repet)
{
azar <- data[rank(runif(n)),  ]
azar[, p] <- as.factor(azar[, p])
parti <- floor(n/nparts)
salida<-rep(0,nparts)
for(j in 1:nparts) {
cc <- ((j - 1) * parti + 1):(j * parti)
if(j == nparts) {
cc <- ((j - 1) * parti + 1):n
}
datap <- azar[cc,  ]
datat <- azar[ - cc,  ]
tempo <- knn(as.matrix(datat[, 1:p - 1]),
as.matrix(datap[, 1:p - 1]), datat[, p], kvec)
salida[j] <- sum(as.numeric(tempo) != as.numeric(datap[, p]))
}
ecv[kk] <- sum(salida)/n
}
errorp=mean(ecv)
}
errorp
}

