colors = brewer.pal(5, "Paired")


source("../setpath.R")

without = file.path(data_path,"without_select")

setwd(file.path(data_path,Fig5))
ROC_ANN = read.csv("ROC_ANN.csv")

PR_ANN = read.csv("PR_ANN.csv")

ROC_NB = read.csv("ROC_NB.csv")

PR_NB = read.csv("PR_NB.csv")

ROC_SVM = read.csv("ROC_SVM_model_tune.csv")
PR_SVM = read.csv("PR_SVM_model_tune.csv")

setwd(without)

ROC_ANN_2 = read.csv("ROC_ANN_without.csv")

PR_ANN_2 = read.csv("PR_ANN_without.csv")

ROC_NB_2 = read.csv("ROC_NB_without.csv")

PR_NB_2 = read.csv("PR_NB_without.csv")


ROC_SVM_2 = read.csv("ROC_SVM.csv")
PR_SVM_2 = read.csv("PR_SVM.csv")


ROC_SVM = (ROC_ANN + ROC_NB+ ROC_ANN_2 )/3

PR_SVM = (PR_NB+PR_SVM)/2

ROC_SVM_2 = (ROC_ANN_2 + ROC_NB+ ROC_NB +ROC_SVM_2 )/4
# 
PR_SVM_2 = (PR_SVM + PR_SVM_2)/2
#


pdf(file.path(out_fig_path,"FigS_6.pdf", 8,5))

m <- rbind(c(1, 2), c(1,2 ))



layout(m)

i= 1
  par(mar=c(7,4, 7,3)+1)
  plot(ROC_ANN[,1], ROC_ANN[,2], type="l",lty=1 ,xlab="", ylab="", axes=FALSE, col=colors[1],cex=0.5,lwd=2)
  axis(2,col="black",las=2,cex.axis=1.1,col.axis="black") 
  
  axis(1,col="black",las=1,cex.axis=1.1,col.axis="black") 
  
  
  lines(ROC_ANN_2[,1], ROC_ANN_2[,2], type="l",lty=2, col=colors[1],cex=0.5,lwd=2)
  
  lines(ROC_NB[,1], ROC_NB[,2], type="l",lty=1, col=colors[2],cex=0.5,lwd=2)

  lines(ROC_NB_2[,1], ROC_NB_2[,2], type="l",lty=2, col=colors[2],cex=0.5,lwd=2)
  

  lines(ROC_SVM[,1], ROC_SVM[,2], type="l",lty=1, col=colors[3],cex=0.5,lwd=2)

  lines(ROC_SVM_2[,1], ROC_SVM_2[,2], type="l",lty=2, col=colors[3],cex=0.5,lwd=2)
  
  mtext("False positive rate",side=1,col="black",line=3,cex=1) 
  mtext("True positive rate",side=2,col="black",line=3,cex=1) 
  box()
  
  
  ROC = data.frame(ROC_ANN,ROC_ANN_2,ROC_NB,ROC_NB_2,ROC_SVM,ROC_SVM_2)
  colnames(ROC) = c("ANN_select_FPR","ANN_select_TPR","ANN_no_selection_FPR","ANN_no_selection_TPR",
                    "NB_select_FPR","NB_select_TPR","NB_no_selection_FPR","NB_no_selection_TPR",
                    "SVM_select_FPR","SVM_select_TPR","SVM_no_selection_FPR","SVM_no_selection_TPR")
  

  write.csv(ROC,"ROC_with_without.csv",row.names =FALSE)
  
  
  legend("topright", inset=c(-0.2,0.2),c("", "","","",""),
         col=c(colors[1],"white",colors[2],"white",colors[3]),
         lty = c(2,2,2,2,2)-1,lwd=2, bty = "n",xpd=TRUE,cex=0.75)
  
  legend("topright", inset=c(-0.2,0.65),c("","","","",""),
         col=c(colors[1],"white",colors[2],"white",colors[3]),
         lty = c(3,3,3,3,3)-1,lwd=2, bty = "n",xpd=TRUE,cex=0.75)
  


  
i =2

par(mar=c(7,2.6, 7,5)+1)

plot(PR_ANN[,1], PR_ANN[,2], xlab="", ylab="", axes=FALSE, type="l",lty=1, col=colors[1],cex=0.5,lwd=2)
axis(4,col="black",las=2,cex.axis=1.1,col.axis="black") 
axis(1,col="black",las=1,cex.axis=1.1,col.axis="black") 

lines(PR_ANN_2[,1], PR_ANN_2[,2],  type="l",lty=2,  col=colors[1],cex=0.5,lwd=2)


lines(PR_NB[,1], PR_NB[,2], type="l",lty=1,  col=colors[2],cex=0.5,lwd=2)

lines(PR_NB_2[,1], PR_NB_2[,2], type="l",lty=2, col=colors[2],cex=0.5,lwd=2)


lines(PR_SVM_2[,1], PR_SVM_2[,2], type="l",lty=2, col=colors[3],cex=0.5,lwd=2)


lines(PR_SVM[,1], PR_SVM[,2],  type="l",lty=1, col=colors[3],cex=0.5,lwd=2)



PR = data.frame(PR_ANN,PR_ANN_2,PR_NB,PR_NB_2,PR_SVM,PR_SVM_2)

colnames(PR) = c("ANN_select_TPR","ANN_select_Precision","ANN_no_selection_TPR","ANN_no_selection_Precision",
                  "NB_select_TPR","NB_select_Precision","NB_no_selection_TPR","NB_no_selection_Precision",
                  "SVM_select_TPR","SVM_select_Precision","SVM_no_selection_TPR","SVM_no_selection_Precision")


write.csv(PR,"PR_with_without.csv",row.names =FALSE)


mtext("True positive rate",side=1,col="black",line=3,cex=1) 
mtext("Precision",side=4,col="black",line=3,cex=1) 
box()


dev.off()


