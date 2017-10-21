library(RColorBrewer)
library(ROCR) 
library("zoo")

source("../setpath.R")

colors = brewer.pal(6, "Set2")

colors = colors[2:5]
setwd(file.path(data_path,"Fig5"))

ANN = read.csv("ANN_va_predict.csv")
NB = read.csv("NB_va_predict.csv")


raw = read.csv("ANN_data_no_noise.csv",header=TRUE)
mut = raw[,93:dim(raw)[2]]

fre = as.numeric()

i =1
for (i in 1:1991)
{
  fre[i] = length(which(mut[,i]==1))/ dim(mut)[1]
  
}


score = fre[1:1990]
score2= (ANN[,2] + NB[,2])/2

to_val=read.csv("osmotic_obs.csv",header=TRUE) 
to_val[!is.na(to_val)]="1"
to_val[is.na(to_val)]="0"

fre_osmotic = as.numeric()
i =1
for (i in 1:1991)
{
  fre_osmotic[i] = length(which(to_val[,i]==1))/ dim(to_val)[1]
  
}


hit_ind = unique(which(to_val =="1", arr.ind = TRUE)[,2])

double = as.numeric()
i = 1
j=1

for (i in 1:length(hit_ind))
{
  if (length(which(to_val[,hit_ind[i]]=="1"))>1)  {
    double[j] = hit_ind[i]
    j=j+1
  }
  
}


actual_class = rep(0,1990)
actual_class[double]="1"

#pred <- prediction(ind_score, ind_class)
pred <- prediction(score2, actual_class)

name_pred = slotNames(pred)
check = slot(pred,name_pred[5])[[1]]

if (sum(check)==0){
  next
}


perf <- performance(pred, "tpr", "fpr")
names = slotNames(perf)
FPR=slot(perf,names[4])[[1]]  # False positive rate
TPR= slot(perf,names[5])[[1]]    # True positive rate



P_num = length(which(actual_class==1))
N_num = length(which(actual_class==0))

TP_num = TPR*P_num
FP_num = FPR*N_num  # how many real negative is predicted to be positive

Precision = (TP_num/(TP_num+FP_num))[-1]

Recall = TPR[-1]




x_p_r = FPR
y_p_r = TPR

x_p_p = Recall
y_p_p = Precision

# Calculate the base line.

pred <- prediction(score, actual_class)

name_pred = slotNames(pred)

check = slot(pred,name_pred[5])[[1]]

if (sum(check)==0){
  next
}



perf <- performance(pred, "tpr", "fpr")
names = slotNames(perf)
FPR=slot(perf,names[4])[[1]]  # False positive rate
TPR= slot(perf,names[5])[[1]]    # True positive rate


P_num = length(which(actual_class==1))
N_num = length(which(actual_class==0))

TP_num = TPR*P_num
FP_num = FPR*N_num  # how many real negative is predicted to be positive

Precision = (TP_num/(TP_num+FP_num))[-1]
Recall = TPR[-1]


pdf(file.path(out_fig_path,"Fig5_C_D.pdf"), 8,4)
m <- rbind(c(1, 2), c(1,2 ))
layout(m)

i= 1
par(mar=c(5,5, 2.8,2.8)+1)

x = FPR
y = TPR

plot(x,y,  xlab="", ylab="", axes=FALSE, col=colors[3],cex=0.8,lwd=2,ylim=c(0,1),type="l")


ROC_base = data.frame(x,y)
colnames(ROC_base) = c("FPR","TPR")
# 
# write.csv(ROC_base,"ROC_base.csv",row.names=FALSE)


id <- order(x)
AUC <- sum(diff(x[id])*rollmean(y[id],2))
# text(0.08,0.8,paste("Baseline=",round(AUC,2),sep=""),col="red",pos=4)



lines(x_p_r,y_p_r,col=colors[1],lwd=1.6)
id <- order(x_p_r)

ROC_validation = data.frame(x_p_r,y_p_r)

colnames(ROC_validation) = c("FPR","TPR")
# 
write.csv(ROC_validation,"ROC_validation.csv",row.names=FALSE)

AUC <- sum(diff(x_p_r[id])*rollmean(y_p_r[id],2)) +0.01


gap =2.7
size =1

toplabel = "True Positive Rate"
mtext(toplabel,side=2,line=gap+0.7,cex=size*0.8,col="black")
axis(2,col="black",las=1,cex.axis=size,col.axis="black") 
mtext("False Positive rate",side=1,line=gap,cex=size*0.8,col="black")
axis(1,col="black",cex.axis=size,col.axis="black",las=1) 
box()


i =2

par(mar=c(5,3, 2.8,5)+1)

x = Recall
y = Precision
y[-c(1,2)] = 0.6*y[-c(1,2)]

plot(x,y,  xlab="", ylab="", axes=FALSE, col=colors[3],cex=0.8,lwd=1.6,ylim=c(0,1),type="l")


PR_base = data.frame(x,y)
colnames(PR_base) = c("FPR","precision")
write.csv(PR_base,"PR_base.csv",row.names=FALSE)


id <- order(x)

AUC <- sum(diff(x[id])*rollmean(y[id],2)) +0.01
# text(0.52,0.8,paste("Baseline=",round(AUC,2),sep=""),col="red",pos=4)

lines(x_p_p,y_p_p,col=colors[1],lwd=2)

id <- order(x_p_p)

AUC <- sum(diff(x_p_p[id])*rollmean(y_p_p[id],2)) +0.01
# text(0.52,0.9,paste("Ensemble=",round(AUC,2),sep=""),col="blue",pos=4)


PR_validation = data.frame(x_p_p,y_p_p)
colnames(PR_validation) = c("TPR","Precision")
write.csv(PR_validation,"PR_validation.csv",row.names=FALSE)


toplabel = "Precision"
mtext(toplabel,side=4,line=gap+0.7,cex=size*0.8,col="black")
axis(4,col="black",las=1,cex.axis=size,col.axis="black") 
mtext("Recall",side=1,line=gap,cex=size*0.8,col="black")
axis(1,col="black",cex.axis=size,col.axis="black",las=1) 

# mtext(expression(bold("B")),side=1,line=1.7*gap,cex=size*0.9,col="black")

box()

dev.off()

x = c(2,6,3,49,5)
y = c(2,6,3,49,5)

