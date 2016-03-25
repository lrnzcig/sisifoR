# FBetaScore
getFBetaScore = function(cm, beta=1) {
  if (dim(cm)[1] != 2 | dim(cm)[2] != 2) {
    return(0)
  }
  tp = cm[2,2]
  fp = cm[2,1]
  fn = cm[1,2]
  precision = tp / (tp+fp)
  recall = tp / (tp+fn)
  result = (1 + beta^2) * precision * recall / ((beta^2) * precision + recall)
  if (is.finite(result) == TRUE) {
    return(result)
  } else {
    return(0)
  }
}
