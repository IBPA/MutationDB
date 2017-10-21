source("../setpath.R")

d1= read.csv(file.path(out_data_path,"clusters.csv"),header=TRUE,stringsAsFactors = FALSE) 


cluster=df[,-1]


colfunc<-colorRampPalette(c("#FF7F50","springgreen","blue"))

colors = colfunc(51)

b=as.vector(t(as.matrix(cluster)))

# map the value of b to the index of the color in the palette
i=1
grid = (max(cluster)-min(cluster))/50

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


yanse = matrix(b,nrow=dim(cluster)[1])
# write.csv(yanse,"colors.csv",row.names=FALSE)
n = dim(cluster)[1]

setwd(out_fig_path)
#plot the first row
# png(file = paste(select, ".png",sep=""),width = 3000, height = 3000, res= 800,
#     ,units = "px", pointsize = 12)

pdf("Fig3A.pdf", height=13, width=13)

margin=5
par(mar=c(margin, margin, margin, margin) + 0.1)

size =0.9
plot(1:n,rep(1,n),col= colors[b[1:n]],pch=".",axes=FALSE,
     xlab="",ylab="",ylim= rev(range(0:nrow(cluster))))

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
