reliefcont <-
function(data,nosample, threshold)
{
# *******************************************
# This program runs Relief for multiple classes
# Uses the function near1 and distancia
# data: name of the dataset
# nosample: number of instances drawn from the original dataset
# threshold: the cutoff point to select the features. First is
#chosen as zero and later is corrected by looking at the relevance plot
#
#Revised: June 2002, revised January 2003, February 2004
#March 03,2004
#Edgar Acuna-Caroline Rodriguez
#************************************************* 

data <- as.matrix(data)
p=dim(data)[2]
f=p-1
#Initializing acum, features, and pesototal
acum<-rep(0,f)
features <- seq(f)
ngroups=length(unique(data[,p]))
pesototal=rep(0,f)
#Number of instances
inst <- length(data[, 1])
#Computing the priors
priors <- tabulate(data[, p])/inst
#Calculating the range of each feature. range=Max-Min
dh <- rep(0, f)
for(j in 1:f) 
 {
  dh[j] <- diff(range(data[, j]))
 }
#Here starts the loop of the 10 repetitions
for (repet in 1:10)
{
#Inilitializating nearhit, pesos and tempo
nearhit <- matrix(0, nosample, f)
pesos <- rep(0, f)
tempo <- matrix(0, ngroups, f)
#Here starts the loop for updating the pesos
for(i in 1:nosample) 
 {
  indices <- sample(inst, 1, replace = TRUE)
  muestra <- data[indices,  ]
  datatemp <- data[ - indices,  ]
  data1=split.data.frame(datatemp[,1:f],datatemp[,p])
  indg <- muestra[p]
  nearhit[i,  ] <- near1(muestra[ - p], data1[[indg]])
 #Finding the nearmiss in each group distinct to the group containing the nearhit 
  for(kk in 1:ngroups) 
   {
    if(kk != indg) 
     {
      nearmiss<- near1(muestra[ - p], data1[[kk]])
     tempo[kk,  ] <- (muestra[ - p] - as.vector(nearmiss))
     }
    for(ii in 1:f) 
     {
      tempo[kk, ii] <- (1/nosample)*(tempo[kk, ii]/dh[ii])^2
     }
   }
  pesomiss <- rep(0, f)
#Updating the pesos for each feature  
for(jj in 1:f) 
   {
    for(kk in 1:ngroups) 
     {
      if(kk != indg) 
       {
        pesomiss[jj] <- pesomiss[jj] + priors[kk] * tempo[kk, jj]
       }
     }
    pesomiss[jj] <- pesomiss[jj]/(1 - priors[indg])
   }
  for(j in 1:f) 
   {
    diff <-  - (1/nosample)*((muestra[j] - nearhit[i, j])/dh[j])^2 + pesomiss[j]
    pesos[j] <- pesos[j] + diff
   }
 }
#print(pesos)
#Normalizing the pesos
#pesos <- pesos/nosample
#selecting the features with pesos greater than a threshold
o1 <- order( - pesos)
o2 <- pesos[o1]
o3 <- o1[o2 > threshold]
#Acumulating the pesos in each repetition

pesototal=pesototal+pesos

#Acumulating the frecuencies of the selected features
acum[o3]=acum[o3]+1

#end of repet
}
#Ordering the total pesos
pesotota=as.matrix(pesototal)
of1 <- order( - pesotota)
of2 <- pesotota[of1]/10
acum=as.matrix(acum)
#Ordering the features according to theirs weights
tabla=cbind(1:f,acum,pesotota/10)
colnames(tabla)=c("feature","frequency","weight")
#print(tabla)
tabla=tabla[order(-tabla[,3]),]
#print(tabla)
cat("Features appearing in at least half of repetitions ordered by their average relevance weight: \n")
print(tabla[tabla[,2]>=5,])
#ploting the total pesos in order to update the threshold 
plot(of2,ylab="weights")
text(1:f,of2,tabla[,1],cex=.7,pos=4)
relevant1=which(acum>=5)
#Selecting the relevant features according to their total pesos and frequencies
relevant2=which(pesotota/10>threshold)
relevant=c(relevant1,relevant2)
#print(relevant)
relevant=relevant[duplicated(relevant)]
#print(relevant)
cat("selected features", "\n")
relevant=tabla[1:length(relevant),1]
#print(tabla[tabla[,1]%in%relevant,])
return(relevant)
}

