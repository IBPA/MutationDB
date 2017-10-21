source("../setpath.R")

d1= read.csv(file.path(data_path,"Database.csv"),header=TRUE,stringsAsFactors = FALSE)

start = grep("rrrD",colnames(d1))

mut = d1[,start:dim(d1)[2]]


# combine conditions
single = unique(d1[,2])

final_m = mut[1:length(single),]

for (i in 1:length(single))
  
{
  index_l = which(d1[,2]==single[i])
  hit_index = unique(which(mut[index_l,]!="",arr.ind=TRUE)[,2])
  final_m[i,hit_index] = "Final" 
}



freq = as.numeric()

for (i in 1:dim(final_m)[2])
  
{
  freq[i]= length(which(final_m[,i]=="Final"))
  
}


fig_2_c = data.frame(colnames(mut),freq)

write.csv(fig_2_c,"fig_2_c.csv",row.names = FALSE)

size =1.2

pdf("Fig2_C.pdf", 8,5);



par(mar = c(5, 5, 4, 4))

b=sort.int(freq,index.return=TRUE,decreasing = TRUE)


plot(b$x[1:4000],type="l",ylab="Frequency",cex.lab=size,cex.axis=size)

cutoff= 16.0486
abline(h=cutoff,col="orange",lty=2)
# abline(v=20)


names = colnames(mut)[b$ix]

out = data.frame(b$x,names)

write.csv(out,"Fig3_C.csv",row.names = FALSE)

colnames(mut)[b$ix[1:20]]
