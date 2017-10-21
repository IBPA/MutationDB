replace_synonym_with_gene_name = function(input_path, output_path,ref_path)

{
  
    cat("start reading files")
  
    df= read.csv(input_path, header=TRUE,stringsAsFactors=FALSE)
    
    ref= read.csv(ref_path, header=TRUE,stringsAsFactors=FALSE)

    a = df[,12]
    p = dim(ref)[1]
    c2 = ref[,2]
    c1 = ref[,1]
    
    # task replace the occurence of c2 with c1 (pairwise)
    i = 1
    for (i in 1:p)
      
    {
      if (length(grep(c2[i],a)!=0)) {
        break
      }
    }
    
    i = 1
    
    for (i in 1:p)
    {
      p1=c("^","^", "^",  ";",";")
      p3 = c("$",";", "/",  ";","$")
      p2 = rep(c2[i],5)
      
      toMatch = paste(p1,p2,p3,sep="")
      
      a = gsub(toMatch[1],c1[i],a)
      a = gsub(toMatch[2],paste(c1[i],";",sep="",collapse=""),a)
      a = gsub(toMatch[3],paste(c1[i],"/",sep="",collapse=""),a)
      a = gsub(toMatch[4],paste(";",c1[i],";",sep="",collapse=""),a)
      a = gsub(toMatch[5],paste(";",c1[i],sep="",collapse=""),a)
    }
    
    df[,10]=a
    setwd(c_path)
    write.csv(df[,1:11],output_path,row.names=FALSE)

}


# bugs. 
# you should keep the mut after substitution
# mut = gsub(ref[i,2],ref[i,1],mut) , Exact match