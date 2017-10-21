source("../setpath.R")

d1= read.csv(file.path(data_path,"Merged_DB.csv"), header=TRUE,check.names=FALSE)


mutator = which(d1[,4]!="")

d1 = d1[-mutator,]




n= dim(d1)[1]
p = dim(d1)[2]
start = grep("rrrD",colnames(d1))

id_db = d1[,3]

d3= read.csv(file.path(data_path,"culture_condition.csv"), header=TRUE,check.names=FALSE)

index2 = match(id_db,d3[,1])
generation = as.numeric(d3[index2,12])



i = 1
rate = as.numeric()

for (i in 1:n)
{
  local = d1[i,start:p]
  rate[i] = length(which(local!=""))/generation[i]
  
}

pdf(file.path(out_fig_path,"Mutation_rate_all.pdf"), 5,4);

par(mar=c(4,5,4,2))

hist(rate,breaks=50,main="",xlab="",ylab="",xlim=c(0,40),ylim=c(0,500))

mtext("Mutation Rate (/generation)",1,line=2.3)
mtext("Frequency",2,line=3)

dev.off()
