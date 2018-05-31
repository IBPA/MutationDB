source("../setpath.R")
df = read.csv(file.path(out_data_path,"MLDB_medium.csv"),check.names = FALSE)


library(e1071)
condition =  read.csv(file.path(out_data_path,"CultureCondition.csv"),check.names = FALSE,
                      stringsAsFactors = FALSE)
ss = paste(colnames(condition), condition[1,], sep= "_")

preds = as.numeric()
for (gene_id in 1:1990) {

    data=df[,c(1:79,79+gene_id)]
    colnames(data)[80] = "gene"
    data[] <- lapply(data, factor)
    test = data[1,1:80,drop=FALSE]
    test[1,] = 0
    test[,ss] = 1
    
    #  model = naiveBayes(gene ~ ., data)
    #save(model,file = paste("../../out_model/NB",gene_id,".RData",sep=""))
    load(paste("../../out_model/NB",gene_id,".RData",sep=""))
    preds[gene_id] = predict(model,test[1,-ncol(test),drop=FALSE],type='raw')[,2]
}

res = data.frame(colnames(df)[79+1:1990], preds)
colnames(res) = c("genome site", "probability")
write.csv(res, file.path(out_pred_path,"pred_NB.csv"), row.names = FALSE)

