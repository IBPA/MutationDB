sort_out_delete_mutation = function(input_file, output_file)
  
{
    df= read.csv(input_file, header=TRUE,stringsAsFactors=FALSE)
    # Replace NA cells with blank
    df[is.na(df)]=""
    
    colnames(df)[10]
    
    # Exclude blank profiles (blank in the gene columns)
    blank = which(df[,10]=="")
    df = df[-blank,]
    
    # mutation type is blank
    colnames(df)[8]
    
    blank = which(df[,8]=="")    # rhsB underwent a long substitution in these files: 18 18 19 20 20 21 21 22 23 23 23 24 25 25 26 27
    df = df[-blank,]
    
    # check the categories 
    index=which(!duplicated(df[,8]))
    cat = df[index,8]
    
    # unify the mutation name
    df[,8]=sub("Amp.*","Amplication",df[,8])
    
    
    df[,8]=sub("IS.*","IS-insertion",df[,8])
    
    
    # df[,8]=sub(".*SNP.*","SNP",df[,8])
    df[,8]=sub("MNP","SNP",df[,8])
    
    df[,8]=sub(".*nserti.*","Insertion",df[,8])
    
    
    df[,8]=sub("del","Deletion",df[,8])
    df[,8]=sub(".*Deletio.*","Deletion",df[,8])
    
    # check the current mutation types
    index=which(!duplicated(df[,8]))
    df[index,8]
    
    df[,10] = sub ("'","",df[,10])
    #Find the row number of each biological replicate

    tag = df[,1]
    
    tag_star = which(!tag==tag[2])
    
    i=1
    
    # check the existence of the file that will hold the sortout gene profile
    
    if (file.exists("append.csv")) {
    file.remove("append.csv") }
    
    track = as.numeric()
    
    # For each replicate sort out the gene and corresponding mutation
    for(i in 1:length(tag_star))
    
    
    {
    
    start = tag_star[i]
    
    # the for loop is a little different at the end, so if and else is used.
    if (i==length(tag_star)) {end = dim(df)[1]
    }else  end = tag_star[i+1]-1 
    
    # The following is applied to a standing alone dataset
    local = df[start:end,]
    
    # extract the index of deletion
    del = grep (".*;.*",local[,10])
    
    bug_row = grep("marC/marR",local[,10])
    
    if (!sum(bug_row)==0){
      bug = i
    }
    
    # create two characters that holding the mutation and genes
    gene =as.character()
    mutation =as.character()
    # extract the sample ID and gene and mutation first 
    gene = c(gene,local[1,1],local[1,2])
    mutation = c(mutation, local[1,1],local[1,2])
    
    # if there are some large deletions, the left one is represented by local[-,10]
    # Otherwise, length(del)==0, local[-0,10] causes a syntax error.
    
    if (length(del) ==0) {
      gene = c(gene, local[,10])
      mutation = c(mutation, local[,8])
    out = data.frame(gene,mutation)
      write.table(t(out),file="append_01_12_3.csv",row.names=FALSE,sep=",",append=TRUE,col.names=FALSE,quote=FALSE)
      #write.table(t(gene),file="append_01_12_3.csv",row.names=FALSE,sep=",",append=TRUE,col.names=FALSE,quote=FALSE)
      # write.table(t(mutation),file="append_01_12_3.csv",row.names=FALSE,append=TRUE,col.names=FALSE,sep=",",quote=FALSE)
      track[i] = length(mutation)
      next
    
    } else {
      gene = c(gene, local[-del,10])
      mutation = c(mutation, local[-del,8])
    
    del_num = length(del)
    # loop through the deletion events
    j=1
    
    for (j in 1:del_num)
    {
    gene_list= unlist(strsplit(local[del[j],10],split=";"))
    gene = c(gene,gene_list)
    mutation = c(mutation,rep(local[del[j],8],length(gene_list)))
    track[i] = length(mutation)
    }
    }   # The ending bracket of if else
    
    if (i ==162) {
      mutation  = gsub("\\n","",mutation)
    }
    
    write.table(t(gene),file=output_file,row.names=FALSE,sep=",",append=TRUE,col.names=FALSE,quote=FALSE)
    write.table(t(mutation),file=output_file,row.names=FALSE,append=TRUE,col.names=FALSE,sep=",",quote=FALSE)
    
    }  # the ending bracket of the outer-most loop.

}





# bugs
# 1. if no duplicated, df[-del,8] does not work
#.2. if no large deletion, we should output first
# 3. the last time of the loop.
# 4. check the existence of an output file
# 5. write.table not write.csv (append does not work in write.csv function)

# 6. if (file.exists()) quotation mark is missing
# 7. close the file before doing any operation
# 8. When append the mutation and gene using write.table function,. Since there is some coma in
#     some entries, something goes wrong.
# 9. The quality control for each mutation, it should be done. But not now. Compare it with existing genes.