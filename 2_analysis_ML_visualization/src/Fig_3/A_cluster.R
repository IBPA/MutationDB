source("../setpath.R")
#required packages (e.g. use install.packages if you dont have them)
require("FSelector")
require("randomForest");
require("glmnet");
require("pls");
require("caret");
require("apcluster");


df=read.csv(file.path(out_data_path,"NB_data.csv"),header=TRUE)

cul_f_last = tail(grep("Pert_",colnames(df)),n=1)
p = dim(df)[2]
mut = df[,(cul_f_last+2):p]

dep= read.csv(file.path(out_data_path,"dependence.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)

n = dim(dep)[1]
dep <- matrix(unlist(dep), ncol = n, byrow = TRUE)



logdep=log(dep)

logdep[which(is.infinite(-logdep))] <- 0 
logdep[logdep<0]=0

dep = logdep
diag(dep) = max(dep)/2

simMatrix = dep



getClusterCenters = function(originalMatrix,clusterCenters)
{
  
  calculatedClusters = apply(originalMatrix,1,function(x) which.min(apply(clusterCenters,1,function(y) sqrt(sum(((x - y)^2))))))
  return(calculatedClusters);
}



# the degree matrix is inversed
d = sqrt(1/rowSums(simMatrix))

#make it normalized
dSquare = d %*% t(d);
#calculate the laplacian
lMatrix = simMatrix * dSquare;

# There are about 80 eigen values close to zero
# hist(eigen(lMatrix)$values)

# output the index after ordered
out_index = matrix(nrow=n,ncol=90)


q=18

# for (q in 17:21)
# {

  centers=q

#now do the spectral embedding
fullSpectX = svd(lMatrix)$u[,1:centers];
#normalize the rows to have unit mean
fullSpectY = fullSpectX/sqrt(rowSums(fullSpectX^2))
#finally calculate the k means
kMeans = kmeans(fullSpectY,centers,iter.max=500,nstart=50);
clusterCenters = kMeans$centers;
clusteredOutput = getClusterCenters(fullSpectY,clusterCenters);

# a stores the cluster index of each point in the adjacent matrix
a= clusteredOutput
# order the data by their cluster number 

index=sort.int(a,index.return=TRUE,decreasing = FALSE)$ix



cluster = dep[index,index]
# 
# write.csv(cluster,"cluster.csv")


 i = 1
 ave_dep = as.numeric()
 
 for ( i in 1:q)
 {
   cul_i = which(a==i)
   local = dep[cul_i,cul_i]
   
   n = dim(local)[2]
   
   ave_dep[i] = sum(colSums(local))/(n*n)
   
 }

clus_index = sort.int(ave_dep,index.return=TRUE,decreasing = FALSE)$ix

max_clus = clus_index[length(clus_index)]

gr = which(a==max_clus)

top_clus = colnames(mut)[gr]



write.csv(data.frame(gr,top_clus),file.path(out_data_path,"top_clus_18.csv"),row.names=FALSE)


order_clu_index = as.numeric()

cluster_label = as.numeric()

i= 1
for (i in 1:q)

{
  order_clu_index = c(order_clu_index,which(a==clus_index[i]))
  
  cluster_label = c(cluster_label,a[which(a==clus_index[[i]])])
  
}
  
cluster=dep[order_clu_index,order_clu_index]

# You plot your self

colfunc<-colorRampPalette(c("coral","springgreen","blue"))
colors = colfunc(51)
# output the dependence matrix before clustering

shuffle = sample(1:n,n,replace=FALSE)
dat=dep[shuffle,shuffle]




# The dependence matrix after the clustering

b=as.vector(t(as.matrix(cluster)))



colnames(cluster) = colnames(mut)[order_clu_index]

write.csv(cluster,file.path(out_data_path,"cluster_18.csv"),row.names = FALSE)



# map the value of b to the index of the color in the palette
i=1
grid = (max(dep)-min(dep))/50

group=list()

for (i in 1:50)
{
  group[[i]] = which(b<grid*i&b>=grid*(i-1)) 
}

top=which(b>=grid*50)
group[[50]]=c(group[[50]],top)

i=1

for (i in 1:50)
{
  check =length(group[[i]])
  if (check!=0){
    b[group[[i]]] = i }
  
}


hist(b)

yanse = matrix(b,nrow=1990)
write.csv(yanse,"colors.csv",row.names=FALSE)

n = dim(cluster)[1]


# 
#plot the first row
png(file = paste("Origin", q, ".png",sep=""),width = 3000, height = 3000, res= 800,
    ,units = "px", pointsize = 12)

margin=3.5
par(mar=c(margin, margin, margin, margin) + 0.1)

size =0.9
plot(1:n,rep(1,n),col= colors[b[1:n]],pch=".",axes=FALSE,
     xlab="",ylab="",ylim=rev(range(0:2000)))

axis(3,xlim=c(2000,0),cex.axis=size)


axis(2,cex.axis=size)
box()

label_space=2
mtext("Mutation",side=3,col="black",line=label_space,cex=size) 
mtext("Mutation",side=2,col="black",line=label_space,cex=size) 

#plot the following row

i=2
for (i in 2:n)
{
  points(1:n,rep(i,n),col=colors[b[(n*(i-1)+1):(n*i)]],pch=".")
  
}

dev.off()

}

