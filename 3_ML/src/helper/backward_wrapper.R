# inputs:
#    data: a data.frame storing all the columns
#    exclude_index: exclude_index so far
#    conditions: the condition id for each row
#    iteration: i, indicating this is the ith iteration
#    
# outputs:
#   updated_exclude_index: append the newly selected column index to the exclude_index
#   the performance improvement:
   
backward_wrapper = function(data,ML_method,hypers)
{ 
  df = data
  survive_index = seq(1,ncol(data)-1,1)

  survive_index = sample(survive_index,55)
  best_perf=0
  best_perfs = as.numeric()
  exclude_index = as.numeric()
  for (i in 2:length(survive_index)-1) {
    cat("excluded: ", exclude_index,"\n")
    cat("best_perfs: ", best_perfs,"\n")
    perfs = as.numeric()
    for(index in survive_index) {
      survive_index_copy = survive_index[!survive_index %in% index]
      res = cross_validation(df[,c(survive_index,ncol(df))],ML_method,hypers)
      perfs = c(perfs,res[["perf"]])
      cat("perfs: ", perfs, "\n")
    }
    max_index = which.max(perfs)
    current_best = max(perfs)
    if (current_best > best_perf & best_perf < 0.97) {
      best_perf = current_best
      best_perfs = c(best_perfs, (best_perf))
      exclude_index = c(exclude_index, (survive_index[max_index]))
      survive_index = survive_index[!survive_index %in% survive_index[max_index]]
    } else {
      break
    }
  }
  index_auc = list(best_perfs,exclude_index)
  return(index_auc)
}

# if(interactive()) {
#   ML_method = "NB"
#   hypers = c(3,50)
#   source("../setpath.R")
#   library(e1071)
#   library(ROCR)
#   df = read.csv(file.path(out_data_path,"MLDB.csv"))
#   data=df[,1:85]
#   colnames(data)[85] = "gene"
#   data[] <- lapply(data, factor)
#   source("./libs/cross_validation.R")
#   source("./libs/compute_AUC.R")
#   
#   cat("ML_method: ", ML_method,"\n")
#   res = backward_wrapper(data,ML_method,hypers)
# 
# }
