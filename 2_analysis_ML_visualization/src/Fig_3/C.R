
#required packages (e.g. use install.packages if you dont have them)
library(stringr)
library(gplots)


gene=as.character()
GO = as.character()

gene_index=as.numeric()
GO_index=as.numeric()

GO_repeat= as.character()

p_values = as.numeric()

p_value=0.015

for (i in c(6,4,1,15,18,12))
  
{
df=read.csv(file.path(out_data_path, paste("David/cluster",i,".csv",sep="")),header=TRUE,stringsAsFactors = FALSE) 

df[,1] = gsub("GO.*~","",df[,1])
survive = df[ df[,2]<p_value,]

if (nrow(survive)>0)

{
a =unique(c(str_split_fixed(survive[,3], ",",10)))
gene = c(gene,a)

newGO = setdiff(survive[,1],GO)

for (each in newGO)
{
aa= which (apply( survive,1, function (x) any(grepl(each,x))))

p_values = c(p_values,survive[aa,2])
GO_repeat = c(GO_repeat,survive[aa,1])
}
GO = c(GO,newGO)

gene_index=c(gene_index,rep(i,length(a)))
GO_index = c(GO_index,rep(i,length(newGO)))

}
}

gene = gsub(" ","",gene)
not_space = gene!=""
gene=gene[not_space]
gene_index = gene_index[not_space]


GO_p_value= data.frame(GO_repeat,p_values)
write.csv(GO_p_value,file.path(out_data_path,"GO/GO_p_values.csv"))
write.csv(GO,file.path(out_data_path,"GO/GO.csv"))

# result


res = matrix(nrow=length(GO),ncol=length(gene))

row.names(res) = GO
colnames(res) = gene




j=1

for (i in c(6,4,1,15,18,12))
  
{
  df=read.csv((file.path(out_data_path, paste("David/cluster",i,".csv",sep=""),header=TRUE,stringsAsFactors = FALSE) 
  
  df[,1] = gsub("GO.*~","",df[,1])
  survive = df[ df[,2]<p_value,]
  

  if (nrow(survive)>0)
    
  {
    for (each in gene)
    { 
      a= which (apply( survive,1, function (x) any(grepl(each,x))))
      hit_GO = survive[a,1]
      
      res[hit_GO,each]=j
    }
    j=j+1
  } # if end
}


res[is.na(res)]=0

plotclr=c("#ECE8DF","#D38968","#E5C370","#C3A7CE","#AA9C8F")

pdf(file.path(out_fig_path,"Fig3_b.pdf"), height=5, width=13)

heatmap.2(x = res,
          dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',
          key = F,
          lmat = matrix(c(4,2,3,1),
                        nrow=2,
                        ncol=2),
          lhei = c(0.1,1.0),
          lwid = c(0.1,2.3),
          #col = cm.colors(256),
          col = plotclr,
          margins=c(4,26),
          cexRow=1.2,cexCol =1,srtCol=90)
dev.off()


res_index <- rbind(gene_index, res)
res_index = cbind(c(0,GO_index),res_index)

write.csv(res_index,"../out_data/GO_gene.csv")
