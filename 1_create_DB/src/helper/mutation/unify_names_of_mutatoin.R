unify_names_of_mutatoin = function(input_path, output_path,ref_path)
{
    df= read.csv(input_path, header=TRUE,stringsAsFactors=FALSE)
    
    # the "Gene' column denotes the mutated genome sites
    mut = df[,'Gene']
    
    
    # replace all the alphanumeric and ; / to blank; then pick out the cell filled with punctuation
    mod = gsub("[[:alnum:][:space:];/_-]","",mut)
    
    pun_id = grep("[[:punct:]]",mod)
    
    
    mut[pun_id] = gsub("'","",mut[pun_id])
    mut[pun_id] = gsub("\\?$","",mut[pun_id])
    mut[pun_id] = gsub("\\?\\[",";",mut[pun_id])
    
    mut[pun_id] = gsub("\\?\\[",";",mut[pun_id])
    
    
    mod = gsub("[[:alnum:][:space:];/_-]","",mut)
    pun_id = grep("[[:punct:]]",mod)
    left = mut[pun_id]
    left = gsub("\\]","",left)
    left = gsub("\\[","",left)
    left = gsub("\\?",";",left)
    
    
    split <- unlist(strsplit(left, split=";"))
    del <- matrix(split, ncol=2, byrow=TRUE)
    
    
    # fill in the deleted genes between two genes according to the reference file
    
    ref= read.csv(ref_path, header=TRUE,stringsAsFactors=FALSE)
    
    gene_list = as.character()
    
    i=1
    for (i in 1:dim(del)[1])
      
    {
      b1 = match(del[i,1],ref[,3])
      b2 = match(del[i,2],ref[,3])
      
      start = min(b1,b2)
      end = max(b1,b2)
      
      if (is.na(end))
      {
        gene_list[i]= paste(del[i,],collapse=";")
        next}
      
      chunk = ref[start:end,3]
      gene_list[i] = paste(chunk,collapse=";")
      
    }
    mut[pun_id]=gene_list
    
    
    out= data.frame(df,mut) 
    
    write.csv(out,output_path,row.names=FALSE)
}
