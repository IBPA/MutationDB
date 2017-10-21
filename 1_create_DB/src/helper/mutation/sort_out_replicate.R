sort_out_replicate = function(input_file, output_file)
{
  
  df= read.csv(input_file, header=TRUE,stringsAsFactors=FALSE)
  df[is.na(df)]=""
  
  tag = df[,1]
  tag_star = which(!tag==tag[2])
  
  
  rank = sub(".*-","",tag[tag_star])
  
  newtag = paste(df[tag_star,3],rank,sep="-")
  
  
  df[tag_star,1] = newtag
  
  i=1
  for (i in 1:(length(tag_star)-1))
  
  {
    subtotal = df[tag_star[i]:(tag_star[i+1]-1),]
    
    single = which(!duplicated(subtotal[,5]))
    if (length(single)==1)
      next
    else {
    
      j=2
    
      for (j in 2: length(single))
      {
      df[single[j]+tag_star[i]-1,1] = paste (subtotal[1,1],"*",j,sep="")
      }
    }
  }
  
  tag = df[,1]
  
  tag_star = which(!tag==tag[2])
  paper_index = sub("-.*","",tag[tag_star])
  
  df[tag_star,11] = paper_index
  
  unique = which(!duplicated(df[,11]))
  
  unique = unique[-2]
  
  i=1
  
  for (i in 1:(length(unique)-1))
  {
     re_index = which(!df[unique[i]:(unique[i+1]-1),1]==df[2,1])
     ab_index = unique[i]+re_index-1
     
     sequ = 1:length(re_index)
    final = paste(sub("-.*","",df[ab_index,1]),sequ,sep="-")
    df[ab_index,1] = final
    
  }
  
  i=length(unique)
  
  re_index = which(!df[unique[i]:(dim(df)[1]),1]==df[2,1])
  ab_index = unique[i]+re_index-1
  
  sequ = 1:length(re_index)
  final = paste(sub("-.*","",df[ab_index,1]),sequ,sep="-")
  df[ab_index,1] = final
  write.csv(df,output_file,row.names=FALSE)
  
}

# bugs
# the gsub is not effective in place you should assign the mut after substituiton to mut