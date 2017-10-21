source("../setpath.R")
library(ggplot2)

df = read.csv(file.path(out_data_path,"Hotspot.csv"),header=TRUE,stringsAsFactors = FALSE)

d3 = df[,c(2,3)]

mean.f = mean(d3[,1], na.rm = T)
var.f = var(d3[,1], na.rm = T)
sd.f = sd(d3[,1], na.rm = T)

cutoff = qgamma(0.95, shape=mean.f^2/var.f, rate= mean.f/var.f)
 
pdf(file.path(out_fig_path,"Fig2_E.pdf"), 9,5);

par(mar = c(5, 5, 4, 4))


size = 1.2
plot(d3[,1],type="l",xlab="Gene/intergenic index",ylab="Count",axes=FALSE,cex.lab=size)
at = c(0,200,400,600,800)
label = c("0","1Mb","2Mb","3Mb","4Mb")

axis(1,las=1,at=at,labels= label, cex.axis=size)
axis(2,cex.axis=size,las=1)
abline(h=cutoff,col="orange",lty=2)
box()

dev.off()
