# inputs:
#    data: a data.frame storing all the columns
# hypers for SVM and neural network. hypers is a vector

# outputs:
#   the performance improvement:
   
cross_validation = function(data,ML_method,hypers)
{ 
      data <- data[sample(nrow(data)),]
      
      score = as.numeric()
      pp = ncol(data)
      n= nrow(data)
      i =1
      for (i in seq(1,nrow(data),2))
      {
        train = data[-c(i,i+1),]
        test = data[c(i,i+1),]
        if (ML_method=="SVM") {
        model = svm(gene~.,data=train,kernel="radial",cost = hypers[1],
                  gamma = hypers[2],probability = TRUE, type = "C")
        score[c(i,i+1)] = attr(predict(model,test[,-pp],probability = TRUE),"probabilities")[,'1']    #decision.values = TRUE
        } 
        if (ML_method=="NB") {
        model = naiveBayes(gene ~ ., train)
        score[c(i,i+1)] = predict(model,test[,-pp],type='raw')[,2]
        }
      }
      res = list("score" = score,"label" = data[,pp])
      return(res)
}

# 
# if(interactive()) {
#       ML_method = "SVM"
#       source("../../setpath.R")
#       library(e1071)
#       library(ROCR)
#       df = read.csv(file.path(out_data_path,"MLDB.csv"))
#       data=df[,1:85]
#       colnames(data)[85] = "gene"
#       data[] <- lapply(data, factor)
#       res = cross_validation(data,"SVM",c(5,50))
#       # cat(res[["perf"]])
# }
