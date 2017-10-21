mut_one_hot_encode = function(input_file, output_file)

{
    col_num = count.fields(input_file, sep = ",")
    which(is.na(col_num))
    max_num = max(col_num)
    df= read.table(input_file, header=FALSE,sep=",",fill=TRUE,col.names=1:max_num,stringsAsFactors=FALSE)
    
    # check the number of mutations 
    # n = dim(df)[1]
    # p = dim(df)[2]
    # data = df[1:n,3:p]
    # 
    # length(which(data=="SNP"))
    # length(which(data!=""))
    gene <- df[seq(1, nrow(df), 2),]
    
    mutation <- df[seq(2, nrow(df), 2),]
    p = dim(gene)[2]
    gene = gene[,3:p]
    
    # as.vector does not work, because it is not a matrix, but a 
    
    gene = as.matrix(gene)
    total = as.vector(gene)
    
    unique_in = which(!duplicated(total))
    
    blank = which(total[unique_in]=="")
    
    final_list = total[unique_in][-blank]
    
    mut_matrix = matrix(nrow=dim(gene)[1],ncol=length(final_list))
    
    mut_matrix[is.na(mut_matrix)]=""
    
    i=1
    
    for (i in 1:dim(gene)[1])
    {
    
    index=match(gene[i,],final_list)
    not_NA = which(!is.na(index))
    real_in = index[not_NA] 
    number =length(real_in)
    mut_matrix[i,real_in] = as.matrix(mutation[i,3:(number+2)])  # the difference between data.frame and matrix
    }
    
    output=data.frame(mutation[,1:2],mut_matrix)
    colnames(output) = c("sample ID","Culture Condition ID", final_list)
    
    if (file.exists(input_file)) {
      file.remove(output_file) }
  
    write.table(output,file=output_file,row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
    

}
