source("setpath.R")

library(e1071)
library(ROCR)

# extract the index of unique condition
# the carbon source and supplemental columns were deleted manually
df= read.table(file.path(data_path, "DB.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)
               
               
conditions=df[,3]
single = which(!duplicated(conditions))
num_c = length(single)
# get the index of the first mutation
mut_star= tail(grep("Pert_",colnames(df)),n=1)+1

n=dim(df)[1]

gene_id=1
time = mut_star+gene_id-1
colnames(df)[time] ="ybcS"

# conditon with one gene to predict

data = df[,c(5:mut_star-1,time)]
pp = dim(data)[2]

c_seq=c(0.1,10)
g_seq=c(0.1,10)
t=0
r = 0
out_perf=as.numeric()

score = as.numeric()

i =1
for (i in 1:num_c)
{
  exclude = which(conditions==conditions[single[i]])
  t_i=setdiff( c(1:n),exclude)
  train = data[t_i,]
  test = data[exclude,]
  
  
  model=svm(ybcS~.,data=train,kernel="radial",cost = c_seq[t+1],
            gamma=g_seq[r+1],probability = TRUE)
  score[exclude] = predict(model,test[,-pp],probability = TRUE)    #decision.values = TRUE
  
}

actual_class <- data[,pp]
pred <- prediction(score, actual_class)

name_pred = slotNames(pred)
check = slot(pred,name_pred[5])[[1]]


if (sum(check)==0)
{
  out_perf[1]= 0
} else {
  
  auc <- performance(pred, "auc")
  names = slotNames(auc)
  auc_value = slot(auc,names[5])[[1]]
  out_perf[1] = auc_value
  
}


complete = data
df = data

q = 2

for (q in 2:15) # mut_star-1
{
  
  j=1
  inner_perf = as.numeric()
  kick_in = as.numeric()
  
  for (j in 1:45)  # dim(df)[2]-1
  {
    
    kickout = sample(seq(2:dim(data)[2]-1),1)
    
    kick_in[j] = kickout
    
    data = df[,-kickout]
    score = as.numeric()
    pp = dim(data)[2]
    i =1
    for (i in 1:num_c)
    {
      exclude = which(conditions==conditions[single[i]])
      t_i=setdiff( c(1:n),exclude)
      train = data[t_i,]
      test = data[exclude,]
      model=svm(ybcS~.,data=train,kernel="radial",cost=c_seq[t+1],
                gamma=g_seq[r+1],probability = TRUE)
      score[exclude] = predict(model,test[,-pp],probability = TRUE)    #decision.values = TRUE
    }
    actual_class <- data[,pp]
    pred <- prediction(score, actual_class)
    name_pred = slotNames(pred)
    check = slot(pred,name_pred[5])[[1]]
    if (sum(check)==0)
    {
      inner_perf [j] = 0
    } else {
      auc <- performance(pred, "auc")
      names = slotNames(auc)
      auc_value = slot(auc,names[5])[[1]]
      inner_perf [j] = auc_value
    }
  }
  
  out_perf[q] = max(inner_perf)
  
  if (out_perf[q]<out_perf[q-1])
  {break
  } else {
    left = kick_in[which.max(inner_perf)]
    df = df[,-left]
  }
  
}

index  = match(colnames(df),colnames(complete))
setwd("/home/wxk/mutation/output/SVM_selection")
write.csv(index,file=paste("select_",gene_id,".csv",sep=""),row.names=FALSE)


# bugs after you change the data to numeric the form of predicion of SVM changes
#  model=svm(ybcS~.,data=train,kernel="radial",cost=c_seq[t+1],
# gamma=g_seq[r+1],probability = TRUE)
#pred_SVM = predict(model,test[,-pp],probability = TRUE)    #decision.values = TRUE

#predicted = attr(pred_SVM,"probabilities")

#if (dim(test)[1]==1) {
 # score[exclude] = predicted[2]        #predicted[2]
#} else {
#  score[exclude] = predicted[,2]          #predicted[,2]
#}

