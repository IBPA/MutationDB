source("../setpath.R")

d1= read.csv(file.path(ref_path,"MGgenelist.csv"), header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)

end = max(d1[,1])

num = end/5000
interval = 5000

combine = as.character()

for (i in 1:num)
  
{
  
  select  = which(d1[,1]>(i-1)*interval & d1[,1]<=i*interval)
  if (i ==num)
  select  = which(d1[,1]>(i-1)*interval & d1[,1]<=dim(d1)[1])
  
  combine[i] = paste(d1[select,3],collapse=",")
  
  
}


# convert to subsequence

df = read.csv(file.path(data_path,"Merged_DB.csv"),header=TRUE,stringsAsFactors = FALSE,check.names=FALSE)

non_mutator = which(df[,4]=="")

df = df[non_mutator,]

df_o = df

colnames(df)= gsub("-$","",colnames(df))
start = grep("rrrD",colnames(df))


df = df[,start:dim(df)[2]]

names = colnames(df)

inter_index = grep("-",colnames(df))
gene_index = grep("-",colnames(df),invert=TRUE)

chunk1 = length(gene_index)
chunk2 = chunk1 + length(inter_index)
chunk3 = chunk2 + length(inter_index)

mut = data.frame(df[,gene_index],df[,inter_index],df[,inter_index],check.names=FALSE)

colnames(mut)[(chunk1+1):chunk2] = gsub("-.*","",colnames(mut)[(chunk1+1):chunk2])
colnames(mut)[(chunk2+1):chunk3] = gsub(".*-","",colnames(mut)[(chunk2+1):chunk3])

names= colnames(mut)

hit = as.numeric()

number = as.numeric()

for (i in 1:dim(mut)[2])
{
  
  if (length(grep(names[i],combine)))
  hit[i] = grep(names[i],combine)[1]
  
  number[i] = length(grep(names[i],combine))
  # cat(grep(names[i],combine),"\n")
  
}
# hit[chunk3]=0

exclude = chunk1+which(hit[(chunk1+1):chunk2] == hit[(chunk2+1):chunk3])


mut = mut[,-exclude]

hit = hit[-exclude]

#write.csv(mut,"Before_combine_column.csv",row.names=FALSE)


# combine columns belonging to the same subsequence
select = which(!is.na(hit))

mut = mut[,select]

location = hit[select]

single = unique(location)
total = length(unique(location))
mut_chunk = mut[,1:total]

for (i in 1:total)
{
  index_l = grep(paste("^",single[i],"$", sep=""),location)
  if (length(index_l)>1)
  {hit_index = unique(which(mut[,index_l]!="",arr.ind=TRUE)[,1]) } else {
    hit_index = unique(which(mut[,index_l]!="",arr.ind=TRUE))
  }

  mut_chunk[hit_index,i] = "Hit" 
  
}
 
colnames(mut_chunk) = single
# 
# write.csv(mut_chunk,"Mut_seq.csv",row.names=FALSE)





# combine conditions
single = unique(df_o[,3])

final_m = mut_chunk[1:length(single),]

for (i in 1:length(single))
  
{
  index_l = which(df_o[,3]==single[i])
  hit_index = unique(which(mut_chunk[index_l,]=="Hit",arr.ind=TRUE)[,2])
  final_m[i,hit_index] = "Final" 
}




freq = as.numeric()

for (i in 1:dim(final_m)[2])
  
{
  freq[i]= length(which(final_m[,i]=="Final"))
  
}



out = data.frame(colnames(final_m),freq)


output = matrix(nrow= length(combine),ncol=2)


output[,1]= 1:length(combine)

non_zero = match(out[,1],output[,1])


output[-non_zero,2] = 0
output[non_zero,2] = out[,2]


final = data.frame(output,combine)


colnames(final)=c("sub_sequence","Frequency","Genes")


write.csv(final,file.path(out_data_path,"Hotspot.csv"),row.names=FALSE)
