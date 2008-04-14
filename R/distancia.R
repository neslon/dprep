`distancia` <-
function(x, y)
{
#****************************************************
# Finds the euclidean distance between
# two vector x and y or the matrix y and the vector x
# ***************************************************
 if(class(y)=="matrix") 
    {
        distancia = drop(sqrt(colSums((x-t(y))^2)))
        distancia= t(distancia)
    }
  else distancia = sqrt(sum((x-y)^2))
 distancia
}

