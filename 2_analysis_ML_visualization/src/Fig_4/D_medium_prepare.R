
source("../setpath.R")

id = read.table(file.path(out_data_path,"Fig4/multiple_id_bio_num.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)
culture = read.table(file.path(data_path,"culture_condition.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)


media  = culture[,5:6]
m_id = id[,1]
c_id = culture[,1]

index = match(m_id,c_id)
m_media = media[index,]

media_c = paste(m_media[,1],m_media[,2],sep="-")

level = names(rev(sort(table(media_c))))


norm_ave = total[(length(total)/2+1):length(total)]
# extract the time information end

conv = as.numeric()
stand = as.numeric()

i = 1
bio_num = list()

number = rev(sort(table(media_c)))[1:14]

for (i in 1:14)
{
  hit = which(media_c == level[i])
  j = 1
  hold = as.numeric()
  for (j in 1:length(hit))
    
  {
    hold = c(hold,norm_ave[[hit[j]]])
  }
  conv[i] = sum(hold)/length(hold)
  stand[i] = sd(hold)
  
  bio_num[[i]] = c(level[i],number[i],conv[i],stand[i],id[hit,2])
}




setwd("C:/Xiaokang/Dropbox_drive/Mutation_DB_08_28_2016/stage 1 prepare the DB/Biological/6")



file.create(file.path(out_data_path,"Fig4/convergence_media.csv"))
fileCon <- file(file.path(out_data_path,"Fig4/convergence_media.csv")
writeLines(unlist(lapply(bio_num, paste, collapse=",")),fileCon)
close(fileCon)

          