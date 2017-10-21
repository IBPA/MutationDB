# inputs:
#    data: a data.frame storing all the columns
#    conditions: the condition id for each row

# outputs:
#   the performance improvement:
   
cross_validation = function(data,conditions,ML_method)
{ 
  
      if (ML_method == "NB")
      {
        data[data==0] = "absent"
        data[data==1] = "present"
      }
      
      if (ML_method == "ANN")
      {
        data["ybcS"] = as.integer(factor(data[,"ybcS"]))
      }
  
  
      single = which(!duplicated(conditions))
      num_c = length(single)
    
      score = as.numeric()
      pp = ncol(data)
      n= nrow(data)
      i =1
      for (i in 1:num_c)
      {
        exclude_rows = which(conditions==conditions[single[i]])
        train_rows=setdiff( c(1:n),exclude_rows)
        train = data[train_rows,]
        test = data[exclude_rows,]
        
        if (ML_method=="SVM") {

        model = svm(ybcS~.,data=train,kernel="radial",cost = 0.1,
                  gamma=0.1,probability = TRUE, type = "C")
        
        score[exclude_rows] = attr(predict(model,test[,-pp],probability = TRUE),"probabilities")[,'Yes']    #decision.values = TRUE
        } 
        
        if (ML_method=="NB") {

        model = naiveBayes(ybcS ~ ., train)
        score[exclude_rows] = predict(model,test[,-pp],type='raw')[,2]
        }
        if (ML_method == "ANN") {
          
        model <- nnet(ybcS~.,data=train,size=9,decay=0.05,maxit=90,trace=T)
        score[exclude_rows] <- predict(model,test,type='raw')  # type='class'
        }
        
      }

      perf = compute_AUC(score,data[,pp])
      res = list("perf" = perf,"score" = score)
      return(perf)
}