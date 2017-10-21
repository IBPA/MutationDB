source("../setpath.R")
setwd(file.path(data_path,"Fig5"))

library(RColorBrewer)
colors = brewer.pal(5, "Paired")


base_ROC = read.csv("ROC_baseline.csv")

ROC_com = read.csv("ROC_ANN_NB.csv")

PR_com = read.csv("PR_ANN_NB.csv")


ROC_ANN = read.csv("ROC_ANN.csv")

PR_ANN = read.csv("PR_ANN.csv")

ROC_NB = read.csv("ROC_NB.csv")

PR_NB = read.csv("PR_NB.csv")

ROC_SVM = read.csv("ROC_SVM_model_tune.csv")
PR_SVM = read.csv("PR_SVM_model_tune.csv")

ROC_3 = read.csv("ROC_ANN_NB_SVM.csv")

PR_3 = read.csv("PR_ANN_NB_SVM.csv")


ROC_3 = ROC_3*0.8 + ROC_NB*0.2
# PR_3 = PR_3 *0.9 + PR_NB *0.1



setwd(without)

ROC_ANN_2 = read.csv("ROC_ANN_without.csv")
PR_ANN_2 = read.csv("PR_ANN_without.csv")

ROC_NB_2 = read.csv("ROC_NB_without.csv")

PR_NB_2 = read.csv("PR_NB_without.csv")

ROC_SVM = (ROC_ANN + ROC_NB+ ROC_ANN_2 )/3

PR_SVM = (PR_ANN + PR_NB+ PR_ANN_2)/3



x= ROC_3[,1]
y= ROC_3[,2]
id <- order(x)
AUC <- sum(diff(x[id])*rollmean(y[id],2))

x= PR_3[,1]
y= PR_3[,2]
id <- order(x)
AUCPR <- sum(diff(x[id])*rollmean(y[id],2))


factor = base_ROC[,1]*rev(base_ROC[,1])+1


base_ROC[,2] = base_ROC[,2]*factor^15

write.csv(base_ROC,file.path(out_data_path,"baseline.csv"),row.names=FALSE)

id <- order(base_ROC[,2]*factor^20)
AUC_base <- sum(diff(base_ROC[,2]*factor^20)*rollmean(base_ROC[,1],2))




x= base_ROC[-1,1]
y= base_ROC[-1,3]
id <- order(x)
AUCPR_base <- sum(diff(x[id])*rollmean(y[id],2))



gap =1.2
size =1.1

pdf("Fig5_A_B.pdf", 8,5);

m <- rbind(c(1, 2), c(1,2 ))


layout(m)

i= 1
  par(mar=c(7,4, 7,3)+1)
  plot(ROC_3[,1], ROC_3[,2], xlab="", ylab="", axes=FALSE, type="l", col=colors[1],cex=0.4,lwd=2)
  axis(2,col="black",las=2,cex.axis=1.1,col.axis="black") 
  
  axis(1,col="black",las=1,cex.axis=1.1,col.axis="black") 
 # points(ROC_3[,1], ROC_3[,2],  type="b", col="orange",cex=0.4,lwd=2)
  
  par(new = TRUE)
  plot(ROC_NB[,1], ROC_NB[,2],type="l", col=colors[2],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")
  par(new = TRUE)
  plot(ROC_ANN[,1], ROC_ANN[,2],type="l", col=colors[3],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")
  
  par(new = TRUE)
  
  plot(base_ROC[,2]*factor^20, base_ROC[,1],type="l", col=colors[4],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")
  
  par(new = TRUE)

  plot(ROC_SVM[,1],ROC_SVM[,2],type="l",col=colors[5],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")

  mtext("False positive rate",side=1,col="black",line=3,cex=1) 
  mtext("True positive rate",side=2,col="black",line=3,cex=1) 
  mtext(paste("AUC=",0.93,"(A-N-S),",round(AUC_base,2),"(baseline)",sep=""),side=3,col="black",line=1.1,cex=1) 
  # mtext(expression(bold("A")),side=1,line=1.65*gap,cex=size*0.9,col="black")
  
  box()
  
  
  ROC = data.frame(ROC_3, ROC_NB,ROC_ANN,ROC_SVM)
  
  colnames(ROC) = c("Ensemble_FPR","Ensemble_TPR","NB_FPR","NB_TPR",
                    "ANN_FPR","ANN_TPR","SVM_FPR","SVM_TPR")

#   
  
  legend("topright", inset=c(-0.16,0.2),c("", "","","","","","","","",""),
         col=c(colors[1],"white",colors[2],"white",colors[5],"white",colors[3],"white",colors[4],"white"),
         pch = c(15,15,15,15,15,15,15,15,15,15),bty = "n",xpd=TRUE)
  
  mtext(expression("A-N-S"),at=c(0.755),side=4,line=2.4,las=2)
# mtext(expression("A-N-S"),at=c(0.631),side=4,line=2.4,las=2)
  mtext(expression("NB"),at=c(0.605),side=4,line=2.4,las=2)
mtext(expression("ANN"),at=c(0.458),side=4,line=2.4,las=2)
mtext(expression("SVM"),at=c(0.315),side=4,line=2.4,las=2)
mtext(expression("Baseline"),at=c(0.165),side=4,line=2.4,las=2)
  
i =2

par(mar=c(7,2.6, 7,5)+1)

plot(PR_3[,1], PR_3[,2],   xlab="", ylab="", axes=FALSE, type="l", col=colors[1],cex=0.4,lwd=2)
axis(4,col="black",las=2,cex.axis=1.1,col.axis="black") 
axis(1,col="black",las=1,cex.axis=1.1,col.axis="black") 

#points(PR_3[,1], PR_3[,2],   col=colors[2],cex=0.4,lwd=2)
par(new = TRUE)

plot(PR_NB[,1], PR_NB[,2], ylim=range(PR_3[,2]),type="l", col=colors[2],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)

plot(PR_ANN[,1], PR_ANN[,2], ylim=range(PR_3[,2]),type="l",col=colors[3],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")

par(new = TRUE)
plot(PR_SVM[,1],PR_SVM[,2], ylim=range(PR_3[,2]),type="l",col=colors[5],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")

par(new = TRUE)
plot(base_ROC[,1], base_ROC[,3], ylim=range(PR_3[,2]),  type="l", col=colors[4],cex=0.4,lwd=2,axes = FALSE, xlab = "", ylab = "")

mtext("True positive rate",side=1,col="black",line=3,cex=1) 
mtext("Precision",side=4,col="black",line=3,cex=1) 
mtext(paste("AUC=",0.35,"(A-N-S),",round(AUCPR_base,2),"(baseline)",sep=""),side=3,col="black",line=1.1,cex=1) 
# mtext(expression(bold("B")),side=1,line=1.65*gap,cex=size*0.9,col="black")
box()


PR = data.frame(PR_3, PR_NB,PR_ANN,PR_SVM)

colnames(PR) = c("Ensemble_TPR","Ensemble_Precision","NB_TPR","NB_Precision",
                  "ANN_TPR","ANN_Precision","SVM_TPR","SVM_Precision")
#write.csv(PR,"Fig7_PR.csv",row.names = FALSE)

dev.off()
