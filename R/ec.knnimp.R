`ec.knnimp` <-
function(data,nomatr=0,k = 10) 
{
#xnom: vector containing the indexes of the nominal variables
#data: matrix containing data
  x <- as.matrix(data)
  N <- dim(x)[1]
  p <- dim(x)[2] 

#Checking if a row has a missing value
nas <- is.na(drop(x %*% rep(1, p)))
#print(sum(nas))
if(sum(nas)==N) stop("Error: All cases have missing values. Cannot compute neighbors.")

#submatrix with complete rows
#matrix needed in case xcomplete has only one row
  xcomplete <- matrix(x[!nas,  ],,p) 
  colnames(xcomplete)=seq(p)

#submatrix of rows with at least one missing value
  xbad <- x[nas,,drop=FALSE ]

#forming logical vector of nominal variables
 xnom=seq(p) %in% nomatr

#Locating the missing values in the missing submatrix 
  xnas <- is.na(xbad)
  xbadhat <- xbad
  for(i in seq(nrow(xbad))) 
  {
    xinas <- xnas[i,  ]
    xbadhat[i,  ] <- nnmiss(xcomplete, xbad[i,  ],xinas,xnom, K = k)
  }
  x[nas,  ] <- xbadhat
  data2 <-x
  return(data2)
}

