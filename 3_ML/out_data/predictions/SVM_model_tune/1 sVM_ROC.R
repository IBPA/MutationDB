library(ROCR) 

df=read.csv("ANN_mutation.csv",header=TRUE) 
ROC = list()
P_R = list()

j =1 

for (j in 1:863)
{
  
  trigger = file.exists(paste("SVM_socre",j,".csv",sep=""))
  if (trigger==FALSE){
    next
  }

  
  j = j+1
score = read.csv(file=paste("SVM_socre",j,".csv",sep=""))
actual_class <- df[,j]


pred <- prediction(score, actual_class)


perf <- performance(pred, "tpr", "fpr")

plot(perf)

out=data.frame(score,actual_class)

write.csv(out,paste("normal",j,".csv",sep=""),row.names=FALSE)

write.csv(out,paste("Inverted.",j,".csv",sep=""),row.names=FALSE)




name_pred = slotNames(pred)

check = slot(pred,name_pred[5])[[1]]

if (sum(check)==0){
  next
}

perf <- performance(pred, "tpr", "fpr")
names = slotNames(perf)
ROC[[2*j-1]]=slot(perf,names[4])[[1]]  # False positive rate
ROC[[2*j]]=slot(perf,names[5])[[1]]    # True positive rate


P_num = length(which(actual_class==1))
N_num = length(which(actual_class==0))

TP_num = ROC[[2*j]]*P_num
FP_num = N_num - (1-ROC[[2*j-1]])*N_num  # how many real negative is predicted to be positive

precision = TP_num/(TP_num+FP_num)

P_R[[2*j]]=precision
P_R[[2*j-1]] =ROC[[2*j]]

cat(j,"\n")
}

#calculate ROC and precision_recall is done

# get rid of null
num_roc = as.numeric()
Notnull = list()

Notnull_PR = list()

j = 1

for (i in 1:length(ROC))
{
  if (!is.null(ROC[[i]]))
  {
    Notnull[[j]]=ROC[[i]]
    Notnull_PR[[j]] = P_R[[i]]
    j = j+1
  }
  
}

# interpolate
inter_roc = list()

inter_PR = list()

i=1


for (i in 1:(length(Notnull)/2))  # 
{
 tryCatch({
  x = Notnull[[2*i-1]]
  y = Notnull[[2*i]]
  interpolate  = approx(x,y)
  #png(file=paste("ROC", i, ".png",sep=""))
  #plot(x,y)
  #dev.off()
  inter_roc[[2*i-1]] = interpolate$x
  inter_roc[[2*i]] = interpolate$y
  
  
  x = Notnull_PR[[2*i-1]][2:length(Notnull_PR[[2*i-1]])]
  
  y = Notnull_PR[[2*i]][2:length(Notnull_PR[[2*i]])]
 
 # png(file=paste("PR", i, ".png",sep=""))
 #plot(x,y)
 #dev.off()
  
  interpolate  = approx(x,y)
  #png(file=paste("inter_PR", i, ".png",sep=""))
  #plot(interpolate$x,interpolate$y,type="l")
  #dev.off()
  
  inter_PR[[2*i-1]] = interpolate$x
  inter_PR[[2*i]] = interpolate$y
  
  },error=function(e){cat(i,"\n","error","\n")})
  
}


# get rid of null that was generated in interpolation
i=1
j = 1
Notnull_in_PR = list()

for (i in 1:length(inter_PR))
{
  if (!is.null(inter_PR[[i]]))
  {
    Notnull_in_PR[[j]]=inter_PR[[i]]
    j = j+1
  }
  
}

i=1
FPR = inter_roc[[1]]
TPR = inter_roc[[2]]

for (i in 2:(length(Notnull)/2))  # 
{
FPR = FPR+inter_roc[[2*i-1]]
TPR = TPR+inter_roc[[2*i]]

}

i=1
Precision = Notnull_in_PR[[2]]
Recall = Notnull_in_PR[[1]]

for (i in 2:(length(Notnull_in_PR)/2))  # 
{
  Precision = Precision + Notnull_in_PR[[2*i]]
  Recall = Recall + Notnull_in_PR[[2*i-1]]
}

FPR = FPR/( length(Notnull)/2)
TPR = TPR/( length(Notnull)/2)

Precision = Precision /(length(Notnull_in_PR)/2)
Recall = Recall / (length(Notnull_in_PR)/2)


ROC_out = data.frame(FPR,TPR)
PR_out = data.frame(Recall,Precision)

setwd("C:/Users/wmmkx_000/Dropbox/Indexed Paper/DB/stage 4 feature selection/Final_plot")

write.csv(ROC_out,"ROC_SVM_model_tune.csv",row.names=FALSE)

write.csv(PR_out,"PR_SVM_model_tune.csv",row.names=FALSE)

#png(file=paste("ROC", ".png",sep=""))
plot(FPR,TPR)
#dev.off()

plot(Recall,Precision)

