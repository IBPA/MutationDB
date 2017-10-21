df=read.csv("NB_data_3.csv",header=TRUE) 

cul_f_last = tail(grep("Pert_",colnames(df)),n=1)
p = dim(df)[2]

mut = df[,(cul_f_last+2):p]

p=dim(mut)[2]
n= dim(mut)[1]


PAB= matrix(nrow=p,ncol=p)

i=1

PA = as.numeric()

library(doParallel)

detectCores()
cl <- makeCluster(8)
registerDoParallel(cl)
getDoParWorkers()

library(foreach)

ptm = proc.time()

for(i in 1:p)
{

rs <- foreach(j = 1:p, .combine = rbind) %dopar% {
  a = which(mut[,i]=="Presence")
  b= which(mut[,j]=="Presence")
  c=length(intersect(a,b))
  return(c(length(a)/n, c/n))
}
PA[i]= rs[1,1]
PAB[i,]=rs[,2]
}

time = proc.time() - ptm 

out=data.frame(PA,PAB)



n= length(PA)
dep = matrix(nrow=n,ncol=n)

i=1
for (i in 1:n)
{
j=1

for (j in 1:n)
{
dep[i,j] = PAB[i,j]/(PA[i]*PA[j])
}

}

diag(dep)=0

write.csv(dep,"dependence.csv",row.names=FALSE)


