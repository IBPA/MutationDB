rm(list = ls())

source("../setpath.R")
library(e1071)

df = read.csv(file.path(out_data_path,"MLDB_medium.csv"),check.names = FALSE)
condition =  read.csv(file.path(out_data_path,"CultureCondition.csv"),check.names = FALSE,
                      stringsAsFactors = FALSE)
ss = paste(colnames(condition), condition[1,], sep= "_")

preds =  as.numeric()
for (gene_id in 1:1990) {
  data=df[,c(1:79,79+gene_id)]
  colnames(data)[80] = "gene"
  data[] <- lapply(data, factor)
  
  test = data[1,1:80,drop=FALSE]
  test[1,] = 0
  test[,ss] = 1
  hyper = c(0.77,0.08)
  #model = svm(gene~.,data=data,kernel="radial",cost = hyper[1],
  #              gamma = hyper[2],probability = TRUE, type = "C")
  #save(model,file = paste("../../out_model/SVM",gene_id,".RData",sep=""))
  load(paste("../../out_model/SVM",gene_id,".RData",sep=""))
  
  pred = attr(predict(model,test,probability = TRUE),"probabilities")[,'1']
  preds[gene_id] = pred
}

res = data.frame(colnames(df)[79+1:1990], preds)
colnames(res) = c("genome site", "probability")
write.csv(res, file.path(out_pred_path,"pred_SVM.csv"), row.names = FALSE)

