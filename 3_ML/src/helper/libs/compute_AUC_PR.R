compute_AUC_PR  = function (probs, labels) {
  rocs = as.numeric()
  prs = as.numeric()
  for (i in 0:1) {
      fg <- probs[labels==1-i]
      bg <- probs[labels==i]
      roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
      rocs = c(rocs, as.numeric(roc['auc']))
      pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
      prs = c(prs, as.numeric(pr['auc.integral']))
  }
  if (rocs[1] > rocs[2]) {
    return(c(rocs[1],prs[1])) } else {
      return(c(rocs[2],prs[2]))
    }
}

if (interactive()) {
  probs = runif(100)
  labels =  sample(c(1,0),100,replace=TRUE)
  compute_AUC_PR(probs, labels)
}