"near2" <-
function(x, data,vnom)
{
nd <- length(data[, 1])
distall <- rep(0, nd)
for(i in 1:nd) {
distall[i] <- distan2(x, data[i,  ],vnom)
}
#print(sort(distall))
ind1 <- order(distall)[1]
near1 <- data[ind1,  ]
near1
}

