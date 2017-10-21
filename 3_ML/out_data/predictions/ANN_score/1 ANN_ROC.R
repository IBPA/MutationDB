library(ROCR) 

df=read.csv("ANN_mutation.csv",header=TRUE) 
ROC = list()
P_R = list()

j =1 

for (j in 1:(dim(df)[2]-1))
{
  
  trigger = file.exists(paste("score",j,".csv",sep=""))
  if (trigger==FALSE){
    next
  }

score = read.csv(file=paste("score",j,".csv",sep=""))
actual_class <- df[,j]


pred <- prediction(score, actual_class)
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


P_R[[2*j-1]]=precision
P_R[[2*j]] =ROC[[2*j]]


}

#save(ROC, file = "ROC.RData")


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


FPR = Notnull[[1]]
TPR = Notnull[[2]]
i=1

PR = Notnull_PR[[1]]

for (i in 1:(length(Notnull)/2-1))
{
  FPR = FPR+Notnull[[2*i+1]]
  TPR = TPR+Notnull[[2*i]]
  PR = PR + Notnull_PR[[2*i+1]]
 # png(file=paste("ROC", i, ".png",sep=""))
#  plot(Notnull[[2*i+1]],Notnull[[2*i]])
 # dev.off()
  
}



FPR = FPR/( length(Notnull)/2)
TPR = TPR/( length(Notnull)/2)

PR = PR/( length(Notnull)/2)



#png(file=paste("ROC", ".png",sep=""))
plot(FPR,TPR)
#dev.off()

plot(TPR,PR)
