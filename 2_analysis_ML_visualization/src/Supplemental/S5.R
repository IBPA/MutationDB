library(RColorBrewer)

source("../setpath.R")

setwd(file.path(data_path,"Fig5"))

colors = brewer.pal(5, "Paired")

ROC_com = read.csv("ROC_ANN_NB.csv")

PR_com = read.csv("PR_ANN_NB.csv")


ROC_ANN = read.csv("ROC_ANN.csv")

PR_ANN = read.csv("PR_ANN.csv")

ROC_ANN_2 = read.csv("ROC_ANN_no_noise.csv")

PR_ANN_2 = read.csv("PR_ANN_no_noise.csv")

pdf("FigS1_5.pdf", 8,5);


m <- rbind(c(1, 2), c(1,2 ))



layout(m)

i= 1
par(mar=c(7,4, 7,3)+1)
plot(ROC_ANN[,1], ROC_ANN[,2], pch=15,  xlab="", ylab="", axes=FALSE, type="l", col=colors[1],cex=0.4,lwd=2)
axis(2,col="black",las=2,cex.axis=1.1,col.axis="black") 

par(new = TRUE)
plot(ROC_ANN_2[,1], ROC_ANN_2[,2], type="l",  col=colors[3],cex=0.4,lwd=2,xlab="",ylab="",axes=FALSE)


axis(1,col="black",las=1,cex.axis=1.1,col.axis="black") 

mtext("False positive rate",side=1,col="black",line=3,cex=1) 
mtext("True positive rate",side=2,col="black",line=3,cex=1) 
box()


legend("topright", inset=c(-0.16,0.18),c("", "","","",""),
       col=c(colors[1],"white",colors[3],"white","white"),pch = c(15,15,15,15,15),bty = "n",xpd=TRUE)

mtext(expression("Noise added"),at=c(0.777),side=4,line=1.8,las=2)
mtext(expression("No noise"),at=c(0.631),side=4,line=1.8,las=2)


i =2

par(mar=c(7,2.6, 7,5)+1)

plot(PR_ANN[,1], PR_ANN[,2], pch=15,  xlab="", ylab="", axes=FALSE, type="l", 
     ylim=c(0,.169),col=colors[1],cex=0.4,lwd=2)
axis(4,col="black",las=2,cex.axis=1.1,col.axis="black") 
axis(1,col="black",las=1,cex.axis=1.1,col.axis="black") 

par(new = TRUE)
plot(PR_ANN_2[,1], PR_ANN_2[,2], type="l",  col=colors[3],
     ylim=c(0,.170),cex=0.4,lwd=2,xlab="",ylab="",axes=FALSE)



mtext("True positive rate",side=1,col="black",line=3,cex=1) 
mtext("Precision",side=4,col="black",line=3,cex=1) 
box()

ROC_PR = data.frame(ROC_ANN,ROC_ANN_2,PR_ANN,PR_ANN_2)
colnames(ROC_PR) = c("ROC_FPR_noise","ROC_TPR_noise","ROC_FPR","ROC_TPR","PR_TPR_noise","PR_precision_noise",
                     "PR_TPR","PR_precision")

write.csv(ROC_PR,"S2_ROC_PR_ANN.csv",row.names = FALSE)

dev.off()

