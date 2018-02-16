rm(list = ls())
require(PRROC)
source("../setpath.R")

library(e1071)
library(ROCR)
source("../helper/libs/compute_AUC_PR.R")
source("../helper/backward_wrapper.R")
source("../helper/libs/cross_validation_raw_res.R")


load(file.path(out_data_path,"feature_selection","selected_features","SVM_features.rds"))
selected_setting = read.csv(file.path(out_data_path,"feature_selection","SVM_perf_feature_selection.csv"),
                            stringsAsFactors = FALSE)

settings = read.csv(file.path(out_data_path,"SVM_NB_setting"),header = FALSE)

df = read.csv(file.path(out_data_path,"MLDB.csv"))
score = as.numeric()
args <- commandArgs(TRUE)
repeat_id  <- args[1]

gene_num = 1990


for (gene_id in 1:gene_num) {

    setting_id = as.numeric(strsplit(selected_setting[gene_id,4],"_")[[1]][6])
    # the first one is NB
    if (setting_id == 1) {setting_id = 2}
    data=df[,c(1:84,84+gene_id)]
    colnames(data)[85] = "gene"
    data[] <- lapply(data, factor)
    selected_index = SVM_removed_features[[gene_id]]
    hypers = settings[setting_id,2:3]    
    

    pp = ncol(data)
    index = sample(1:nrow(data),nrow(data)*2,replace = TRUE)
    train = data[index,]
    test = data[2,-pp,drop=FALSE]
    
    model = svm(gene~.,data=train,kernel="radial",cost = hypers[1],
                gamma = hypers[2],probability = TRUE, type = "C")
    score[gene_id] = attr(predict(model,test,probability = TRUE),"probabilities")[,'1']    #decision.values = TRUE
    
}


out = data.frame(colnames(df)[85:(84+gene_num)], score)
colnames(out) = c("gene","pred")

write.csv(out, file.path(out_data_path,"SVM_validation",paste("prediction_",repeat_id,".csv",sep="")), row.names = FALSE)    

