disc.1r <-
function (data, convar, binsize = 6)
{
#*************************************************
# This function performs dicretization using the
#Holte's 1R algorithm
#data: the data matrix
#convar: vector of continuous variables
#************************************************
data=as.matrix(data)
if (dim(data)[2]==1) {
stop ("You need class labels for your data.")
}
ncol = dim(data)[2]
nrow = dim(data)[1]
class = ncol
for (i in convar) {
id = matrix(1:nrow, ,1)
discdata = cbind(id,data[,i])
discdata = cbind(discdata,data[,class])
discdata = discdata[order(discdata[,2]),]
discrete = rep(0,nrow)
discdata = cbind(discdata, discrete)
modelist = list()
kk = 1
cc = 1
j = 1
nclass = length(levels(factor(data[,class])))
maxclass = max(data[,class])
freqc=rep(0,maxclass)
sw = 0
while (j <= nrow) {
if (kk <= binsize) {
discdata[j,4] = cc
freqc[discdata[j,3]] = freqc[discdata[j,3]] + 1
if (j == nrow) {
modclass = which(freqc==max(freqc))
modelist= unlist(list(modelist,
modclass[1]))
}
j = j + 1
kk = kk + 1
}
else {
if (sw == 0) {
modclass = which(freqc==max(freqc))
modelist = unlist(list(modelist,modclass[1]))
sw = 1
}
if (discdata[j,3] == modclass[1]) {
discdata[j,4] = cc
j = j + 1
}
else {
cc = cc + 1
discdata[j,4] = cc
kk = 2
freqc = rep(0, maxclass)
freqc[discdata[j,3]] =
freqc[discdata[j,3]] + 1
if (j == nrow) {
modelist=unlist(list(modelist,discdata[j,3]))
}
j = j + 1
sw = 0
}
}
}
gg = 1
for (m in 2:length(modelist)) {
if (modelist[m] == modelist[m - 1]) {
discdata[discdata[,4]== m, 4] = gg
}
else {
gg = gg + 1
discdata[discdata[,4]==m, 4] = gg
}
}
data[,i] = discdata[order(discdata[,1]),4]
}
return (data)
}

