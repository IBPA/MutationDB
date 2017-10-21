source("../setpath.R")


id = read.table(file.path(out_data_path,"Fig4/multiple_id_bio_num.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)
culture = read.table(file.path(data_path,"culture_condition.csv"), header=TRUE,sep=",",fill=TRUE,stringsAsFactors=FALSE)


t_id = grep("Stress",colnames(culture))
stress = culture[,t_id]

m_id = id[,1]
c_id = culture[,1]

index = match(m_id,c_id)
m_stress = stress[index]

level = names(rev(sort(table(m_stress))))

norm_ave = total[(length(total)/2+1):length(total)]
# extract the time information end

number = rev(sort(table(m_stress)))[1:5]



bio_num = list()
conv = as.numeric()
stand = as.numeric()
i = 1


for (i in 1:5)
{
  hit = which(m_stress == level[i])
  j = 1
  hold = as.numeric()
  for (j in 1:length(hit))
    
  {
    hold = c(hold,norm_ave[[hit[j]]])
  }
  conv[i] = sum(hold)/length(hold)
  stand[i] = sd(hold)
  
  stand[1] = stand[1]/2
 
  bio_num[[i]] = c(level[i],number[i],conv[i],stand[i],id[hit,2])
   
}

bio_num[[2]][1] = "No_stress"



file.create(file.path(out_data_path,"Fig4/convergence_stress.csv"))
fileCon <- file(file.path(out_data_path,"Fig4/convergence_stress.csv"))
writeLines(unlist(lapply(bio_num, paste, collapse=",")),fileCon)
close(fileCon)

