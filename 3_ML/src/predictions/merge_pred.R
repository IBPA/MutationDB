source("../setpath.R")
ANN_pred = read.csv(file.path(out_pred_path,"pred_ANN.csv"))
SVM_pred = read.csv(file.path(out_pred_path,"pred_SVM.csv"))
NB_pred = read.csv(file.path(out_pred_path,"pred_NB.csv"))

preds = ANN_pred
preds[,2] = (1/3)*(ANN_pred[,2]+SVM_pred[,2]+NB_pred[,2])
colnames(preds) = c("genome site", "probability")
write.csv(preds, file.path(out_pred_path,"pred_average.csv"), row.names = FALSE)

