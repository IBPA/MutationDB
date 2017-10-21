source("setpath.R") 
output_file = file.path(out_data_path,"Database","DB.csv")


df= read.table(file.path(out_data_path,"mutation","one_hot.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)

colnames(df) = sub("\\.","-",colnames(df))

culture = read.table(file.path(out_data_path,"culture_condition","culture_matrix.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)


# quality control

j=1
indicator = as.numeric()

p=dim(df)[2]

for (j in 3:p)
{
  indicator[j-2] = sum(which(!df[,j]==""))
} 

blank = which(indicator==0)
if (length(blank) ==0) {
cat("everything is good")
}
# quality control check the blank, it should be zero

# culture[is.na(culture)]=""

# culture = as.matrix(culture)

sampleID = df[,2]

cultureID = culture[,1]

# index store the index in cultureID
index=match(sampleID,cultureID)


# no_time store the index of NA elements in the index
no_time = which(is.na(index))

seq = 1:length(index)

#profile_index stores the index in the culture condition 

profile_index = index[seq[-no_time]]

cmatrix = matrix (nrow=length(profile_index), ncol=dim(culture)[2]-2)

cmatrix = culture[profile_index,]

mutation = df[seq[-no_time],]

sample_ID = mutation[,1]

culture_condition_ID = mutation[,2]

cul = cmatrix[,4:dim(cmatrix)[2]-1]

muta_f = mutation[,3:dim(mutation)[2]]

output = data.frame(sample_ID,culture_condition_ID,cul,muta_f)

# there is a NA column in the culture matrix

output[is.na(output)]=""

if (file.exists(output_file)) {
  file.remove(output_file) }


j=1
indicator = as.numeric()

p=dim(output)[2]

for (j in 1:p)
{
  indicator[j] = sum(which(!output[,j]==""))
} 
blank = which(indicator==0)

out = output[,-blank]

replicate = read.csv(file.path(data_path,"culture_condition.csv"), header=TRUE,stringsAsFactors=FALSE)

rep = replicate[,27:28]
rep_NO = replicate [,1]

re_index = replicate[,1]
out_index = out[,2]

index=match(out_index,re_index)

rep_num = rep[index,1]*rep[index,2]
final = data.frame(rep_num,out)

colnames(final) = sub("\\.","-",colnames(final))



write.csv(final,file=output_file,row.names=FALSE)

