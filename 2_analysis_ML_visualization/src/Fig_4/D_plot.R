source("../setpath.R")

df= read.csv(file.path(out_data_path,"Fig4/D.csv"), header=TRUE,stringsAsFactors=FALSE)



pdf(file.path(out_fig_path,"Fig4_D.pdf"), 5,4)

par(mar=c(6,3, 2,5)+1)

colors = c("cyan","cyan","cyan","white","aquamarine","aquamarine","aquamarine","white","deepskyblue","deepskyblue")

border_colors = c(rep("black",length(colors)))

border_colors[c(4,8)]="white"

labels <- df[,1]
mp <- barplot(df[,3], axes = FALSE, axisnames = FALSE,col=colors,
              border= border_colors, ylim= c(0,0.9),cex.axis=0.2)

text(mp, par("usr")[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.7,cex.lab=0.7)

text(mp-0.1, df[,3]+df[,4]+0.03, labels =c("44","11","5","","51","11","5","","57","14"), 
     srt = 0, adj = c(0,0), xpd = TRUE, cex=.7,cex.lab=0.7)

axis(2,cex.axis=0.7)



segments( mp, df[,3]-df[,4],mp,  df[,3]+df[,4],col="black")

arrows(mp, df[,3]-df[,4],mp,  df[,3]+df[,4], lwd = 0.8, angle = 90,
       code = 3, length = 0.05)


legend("topright", inset=c(-0.25,0.4), 
       xpd=TRUE,legend=c("Medium","Strain","Stress"),
       pch=c(15,15,15),ncol=1, col=c("cyan","aquamarine","deepskyblue"),
       bty="n",cex=0.7)

dev.off()