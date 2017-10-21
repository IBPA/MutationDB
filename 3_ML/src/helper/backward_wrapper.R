# inputs:
#    data: a data.frame storing all the columns
#    exclude_index: exclude_index so far
#    conditions: the condition id for each row
#    iteration: i, indicating this is the ith iteration
#    
# outputs:
#   updated_exclude_index: append the newly selected column index to the exclude_index
#   the performance improvement:
   
backward_wrapper = function(data,exclude_index, conditions,iteration = 1,ML_method)
{ 
    all_index = seq(1,ncol(data)-1,1)
    left_index = setdiff(all_index, exclude_index)
    
    perf = as.numeric()
    
    # if this is the first iteration, left_index is set to store one column index number
    if (iteration==1)
    {
     left_index = c(1)
    } 
            
    for (j in left_index)#length(left_index))
    {
      kickout = sample(left_index,1)
      exclude_index_temp = union(exclude_index, kickout)
      
      data_temp = data[,-exclude_index_temp]
      # if it is the first  iteration, remove no features    
      if (iteration==1)
      {
      data_temp = data  
      }
      perf [j] = cross_validation(data_temp,conditions,ML_method)
    }

    if (iteration==1)
    {
      updated_exclude_index = exclude_index
    } else {
    updated_exclude_index = c(exclude_index, which.max(perf))
    }
    
    max_auc = max(perf,na.rm=TRUE)
    
    index_auc = list( updated_exclude_index, max_auc)
    return(index_auc)

}
