"assig" <-
function(x, points, nparti, n) 
{

   x1<-x

   if(nparti==1){ x1[1:n]= 1}

   for(i in 1:nparti) {
      if(i==1) {
idx <- as.numeric(x<points[i])*seq(1,length(x))
x1[idx] <- 1
      }

      if(i==nparti) {
idx <- as.numeric(x>=points[i-1])*seq(1,length(x))
x1[idx] <- nparti
      }

      if((i!=1)&(i!=nparti)) {
idx <- as.numeric((x >= points[i-1])&(x<points[i]))*seq(1,length(x))
x1[idx] <- i
      }

   }
  x1
}

