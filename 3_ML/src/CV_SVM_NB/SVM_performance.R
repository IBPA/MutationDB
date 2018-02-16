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

aucs = as.numeric()
prs = as.numeric()
for (gene_id in 1:1990) {

    setting_id = as.numeric(strsplit(selected_setting[gene_id,4],"_")[[1]][6])
    # the first one is NB
    if (setting_id == 1) {setting_id = 2}
    data=df[,c(1:84,84+gene_id)]
    colnames(data)[85] = "gene"
    data[] <- lapply(data, factor)
    
    selected_index = SVM_removed_features[[gene_id]]
    hyper = settings[setting_id,2:3]
    
    res = cross_validation(data[,-selected_index],"SVM",hyper)
    perf = compute_AUC_PR(res$score, res$label) 
    aucs[gene_id] = perf[1]
    prs[gene_id] = perf[2]
    
    
    write.csv(res$score, file.path(out_data_path,"SVM_prediction",paste("SVM_",gene_id,".csv",sep="")), row.names = FALSE)    

    if(gene_id%%10 ==0) { cat(gene_id)}
    
}

SVM_perf = data.frame(aucs,prs)

write.csv(SVM_perf, file.path(out_data_path,"NB_SVM_perf","SVM_perf.csv"))