# This function is to count the number of unqiue conditions under which a gene is mutated in the
# merged mutation database. 
# 
# Inputs: 
#     col: a col of mutation
#     cons: the condition index of each row

count_condition = function(col,cons) {
  con_index = cons[which(col!="")]
  return(length(unique(con_index)))
}

