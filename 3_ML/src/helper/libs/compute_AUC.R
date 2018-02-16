library(ROCR)

# This function is to compute the AUC of a prediction.
# The inputs are
#           probs: predictions in probability
#           labels: true labels
# output:
#           AUC
# 
# core functions used:
#             prediction: a function used in ROCR library, computing TPR, FPR and so on given various threshold
#             performance: compute AUC

compute_AUC  = function (probs, labels,debug = FALSE)
{
    evaluation <- prediction(probs, labels)
    name_evaluation = slotNames(evaluation)
    check = slot(evaluation,name_evaluation[5])[[1]]
    if (sum(check)==0)
    {
      return(0)
    } else {
      
      if (!debug){
      roc.perf = performance(evaluation,measure = "tpr",x.measure = "fpr")
      plot(roc.perf)
      }
      
      auc <- performance(evaluation, "auc")
      names = slotNames(auc)
      auc_value = slot(auc,names[5])[[1]]
      if (auc_value < 0.5) {
        res = 1 - auc_value
      } else {res = auc_value}
      return(res)
      
    }

}


if (interactive()) {
  probs = runif(100)
  labels =  sample(c(1,2),100,replace=TRUE)
  # cat(compute_AUC (probs, labels,!interactive()))
}