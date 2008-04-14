`midpoints` <-
function(x)
{
  n <- length(x)

  points <- .C("Points",as.double(x),as.integer(n),mpoint = double(n),PACKAGE="dprep")

  points$mpoint
}

