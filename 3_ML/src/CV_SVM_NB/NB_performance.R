rm(list = ls())
require(PRROC)
source("../setpath.R")

library(e1071)
library(ROCR)
source("../helper/libs/compute_AUC_PR.R")
source("../helper/backward_wrapper.R")
source("../helper/libs/cross_validation_raw_res.R")


load(file.path(out_data_path,"feature_selection","selected_features","NB_features.rds"))


df = read.csv(file.path(out_data_path,"MLDB.csv"))
aucs = as.numeric()
prs = as.numeric()

for (gene_id in 1:1990) {

    data=df[,c(1:84,84+gene_id)]
    colnames(data)[85] = "gene"
    data[] <- lapply(data, factor)
    
    selected_index = NB_removed_features[[gene_id]]
    res = cross_validation(data[,-selected_index],"NB",c(0,0))
  #  perf = compute_AUC_PR(res$score, res$label) 
    
    write.csv(res$score, file.path(out_data_path,"NB_prediction",paste("NB_",gene_id,".csv",sep="")), row.names = FALSE)    
    
    
    aucs[gene_id] = perf[1]
    prs[gene_id] = perf[2]
    if(gene_id%%10 ==0) { cat(gene_id)}
}


NB_perf = data.frame(aucs,prs)

write.csv(NB_perf, file.path(out_data_path,"NB_SVM_perf","NB_perf.csv"))