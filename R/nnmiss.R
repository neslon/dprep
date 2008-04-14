`nnmiss` <-
function(x, xmiss, ismiss,xnom, K=1) 
{
#x:submatrix of complete rows from original matrix
#xmiss: a row with a missing value
#ismiss: vector that indicates whether a value in xmiss is missing or not
#xnom: vector with indexes of nominal variables

#Find distance between xmiss (not NA) and each row of x
  xd <- scale(x, xmiss, FALSE)[, !ismiss]
  col=length(xmiss)-sum(is.na(xmiss))
  xd=matrix(xd,,col)
  dd <- drop(xd^2 %*% rep(1, ncol(xd)))

#order of the rows of x according to their closeness to xmiss
  od <- order(dd)[seq(K)]
#if column of ismiss is nominal, find mode if not find mean of KNN
  
  ismiss.nom=ismiss[]&xnom[]
  ismiss.con=ismiss[]&!xnom[]
  xmiss[ismiss.nom] <- as.numeric(moda(x[od, ismiss.nom, drop = FALSE])[1])
  xmiss[ismiss.con] <- drop(rep(1/K, K) %*% x[od, ismiss.con, drop = FALSE])
  xmiss
}

