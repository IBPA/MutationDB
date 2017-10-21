source("../setpath.R")
library(ggplot2)

df = read.csv(file.path(out_data_path,"Hotspot.csv"),header=TRUE,stringsAsFactors = FALSE)

d3 = df[,c(2,3)]

size=1

pdf(file.path(out_fig_path,"Fig2_D.pdf"), 5,5)

par(mar = c(5, 5, 4, 4))


yhist = hist(d3[,1],breaks = 45, freq = F, main = '',
             ylim=c(0,0.16),las=1,cex.axis=size,cex.lab=size,
             ylab="Frequency",xlab="Count",lwd=1,las=1)

box()


curve(dgamma(x, shape =mean(d3[,1])^2/sd(d3[,1]^2), scale = sd(d3[,1])^2/mean(d3[,1])^2), add = T,col="darkorchid")


curve(dgamma(x, shape = mean.f^2/var.f, scale = var.f/mean.f), add = T,col="darkorchid")


abline(v=16.1,col = "orange",lty=2)
dev.off()



