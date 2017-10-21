source("../setpath.R")

library(e1071)
library(ROCR)
library(nnet)
source("../helper/libs/compute_AUC.R")
source("../helper/backward_wrapper.R")
source("../helper/libs/cross_validation.R")

args = commandArgs(TRUE)
gene_id = as.integer(args[1])
ML_method = args[2]


# extract the index of unique condition
# the carbon source and supplemental columns were deleted manually
df= read.table(file.path(out_data_path,"database","DB_1992.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)
               
               
conditions=df[,3]
single = which(!duplicated(conditions))
num_c = length(single)
# get the index of the first mutation
mut_star= tail(grep("Pert_",colnames(df)),n=1)+1

n=dim(df)[1]
time = mut_star+gene_id-1
colnames(df)[time] ="ybcS"

# conditon with one gene to predict

data = df[,c(5:mut_star-1,time)]
data[is.na(data)] = 0
hit_index = which(data[,ncol(data)] !="")
data[hit_index, ncol(data)] = "Yes"
data[-hit_index,ncol(data)] = "No"

perf = cross_validation(data,conditions,ML_method)

exclude_index = as.numeric()
aucs = as.numeric()

for (i in 1:50)
{
cat(i," round")
index_auc = backward_wrapper(data,exclude_index, conditions,iteration=i,ML_method)
exclude_index = index_auc[[1]]

aucs = c(aucs, index_auc[[2]])
if (i > 1 && aucs[i] < aucs[i-1]) {
 cat("stop early")
 break 
 }

}

exclude_index = c(0,exclude_index)
out = data.frame(exclude_index, aucs)

out_file = paste("exclude_index_auc_",gene_id,".csv", sep="")
head(out)
cat(file.path(out_data_path,"feature_selection",ML_method,out_file))
write.csv(out,file = file.path(out_data_path,"feature_selection",ML_method,out_file),row.names=FALSE)
