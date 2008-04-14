`disc.mentr` <-
function(data,vars) 
{
  n <- dim(data)[1]
  p <- dim(data)[2]
  pp <- length(vars)

  data1 <- data

  for(j in 1:(pp-1)) {
     var <- vars[j]
     sal <- discretevar(data,var,n,p)
     nparti <- sal[1]
     points <- sal[-1]
     d <- assig(data[,var],points,nparti,n)
     data1[,var]<-d
  }
  return(data1)
}

