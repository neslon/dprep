"cv10mlp" <-
function(data,units,decay=0,maxwts=1000,maxit=100,repet)
{
#This function finds an estimate for the
#misclasification error through 10-fold cross
#validation for the multilayer perceptron neural network
#Requires the nnet function from Ripley's "nnet" library
# data: the dataset to be used
# units= The number of units in the hidden layer
#decay= The weight decay parameter
#maxwts= The maximum number of weights
#maxit= The maximum numer of iterations
# repet: the number of repetitions
require(nnet)
n <- dim(data)[1]
p <- dim(data)[2]
ecv<-rep(0,repet)
for(kk in 1:repet)
{
azar <- data[rank(runif(n)),  ]
azar[, p] <- as.factor(azar[, p])
parti <- floor(n/10)
salida<-rep(0,10)
for(j in 1:10) {
cc <- ((j - 1) * parti + 1):(j * parti)
if(j == 10) {
cc <- ((j - 1) * parti + 1):n
}
datap <- azar[cc,  ]
datat <- azar[ - cc,  ]
clasest=class.ind(azar[-cc,p])
tempo <- nnet(as.matrix(datat[, 1:(p - 1)]),clasest, entropy=TRUE,size=units,decay=decay,MaxNWts=maxwts,maxit=maxit)
tempo1=predict(tempo,datap)
pd=max.col(tempo1)
salida[j] <- sum(pd!= datap[, p])
}
ecv[kk] <- sum(salida)/n
}
cat("The misclassification errors of each repetition are:","\n")
print(ecv)
cat("The mean misclassifcation error is","\n")
ECV1=mean(ecv)
ECV1
}

