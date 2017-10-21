source("../setpath.R")


d1= load(file.path(out_data_path,"global_pairwise_bio_box.RData"))


library("psych")

global = as.numeric()
global_sd = as.numeric()


for (i in 1:8)
  
{
  global[i] = mean(b_global[[i]])
  global_sd[i] = sd(b_global[[i]])
}


pdf(file.path(out_fig_path,"Fig4_B.pdf"), 5,3.8);

par(mar=c(3,4, 1.2,1.2)+1)

x = 1:length(global)
y = global
sd = global_sd *0.8
plot(x,y,pch=0,  xlab="", ylab="", axes=FALSE, col="black",cex=1,lwd=1.2,ylim=c(-0.05,1))


segments(x, y-sd,x, y+sd,col="black",lwd=0.5)
epsilon = 0.2
segments(x-epsilon,y-sd,x+epsilon,y-sd,col="black",lwd=0.5)
segments(x-epsilon,y+sd,x+epsilon,y+sd,col="black",lwd=0.5)

gap =2
size =1
xtick_l = seq(1,8,1)
xtick = b_x_tick

toplabel = "Global Overlap Ratio"
mtext(toplabel,side=2,line=gap+0.7,cex=size*0.9,col="black")
axis(2,col="black",las=1,cex.axis=size,col.axis="black") 
mtext("The number of replicates",side=1,line=gap,cex=size*0.9,col="black")

axis(1,col="black",cex.axis=size,col.axis="black",at=xtick_l,labels=xtick,las=1) 

box()

dev.off()


global = as.numeric()
global_sd = as.numeric()


for (i in 1:8)
  
{
  global[i] = mean(b_pairwise[[i]])
  global_sd[i] = sd(b_pairwise[[i]])
}

pdf(file.path(out_fig_path,"Fig4_C.pdf"), 5,3.8);


par(mar=c(3,4, 1.2,1.2)+1)

x = 1:length(global)
y = parallel
sd = parallel_sd *0.8
plot(x,y,pch=0,  xlab="", ylab="", axes=FALSE, col="black",cex=1,lwd=1.2,ylim=c(0,1))


segments(x, y-sd,x, y+sd,col="black",lwd=0.5)
epsilon = 0.2
segments(x-epsilon,y-sd,x+epsilon,y-sd,col="black",lwd=0.5)
segments(x-epsilon,y+sd,x+epsilon,y+sd,col="black",lwd=0.5)


toplabel = "Pairwise Overlap Ratio"
mtext(toplabel,side=2,line=gap+0.7,cex=size*0.9,col="black")
axis(2,col="black",las=1,cex.axis=size,col.axis="black") 
mtext("The number of replicates",side=1,line=gap,cex=size*0.9,col="black")
axis(1,col="black",cex.axis=size,col.axis="black",at=xtick_l,labels=xtick,las=1) 



box()

dev.off()