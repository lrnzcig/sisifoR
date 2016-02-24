################################
# utilities for generating learning curves
# for models:
# - glmnet
# - tree (decision trees)
# - party (decision trees)
################################


################################
# learning curves glmnet
# - family binomial
# - applies regularization at every step,
#   selects lambda.1se
################################
learning_curve_glmnet = function(df, formula, results_column_name, number_of_steps=5, seed=13) {
  set.seed(seed)
  folds.0 <- df
  threshold <- 0.5
  errors=matrix(NA, number_of_steps, 2)
  folds.0$ALEATORIO <- floor(runif(dim(folds.0)[1], min=0, max=number_of_steps+2))
  
  for (step in 0:(number_of_steps-1)) {
    print(paste("step", step))
    
    folds.0.x <- model.matrix(formula, data=folds.0)
    folds.0.x.train <- folds.0.x[folds.0$ALEATORIO <= step,]
    folds.0.x.test <- folds.0.x[folds.0$ALEATORIO > step+1,]
    folds.0.y.train <- folds.0[folds.0$ALEATORIO <= step, results_column_name]
    folds.0.y.test <- folds.0[folds.0$ALEATORIO > step+1, results_column_name]
    
    folds.0.cv.lasso=cv.glmnet(folds.0.x.train, folds.0.y.train, alpha=1, family="binomial")
    folds.0.cv.lasso.preds.train = predict(folds.0.cv.lasso, folds.0.x.train, s=folds.0.cv.lasso$lambda.1se, type="response")
    folds.0.cv.lasso.preds.train.conv = ifelse(folds.0.cv.lasso.preds.train > threshold, TRUE, FALSE)
    errors[step+1, 1] <- mean(folds.0.cv.lasso.preds.train.conv != folds.0.y.train)
    folds.0.cv.lasso.preds.test = predict(folds.0.cv.lasso, folds.0.x.test, s=folds.0.cv.lasso$lambda.1se, type="response")
    folds.0.cv.lasso.preds.test.conv = ifelse(folds.0.cv.lasso.preds.test > threshold, TRUE, FALSE)
    errors[step+1, 2] <- mean(folds.0.cv.lasso.preds.test.conv != folds.0.y.test)
  }
  par(mfrow=c(1,1))
  plot(errors[,1], type="l", col="blue", ylim=c(min(errors), max(errors)))
  lines(errors[,2], type="l", col="red")
  return(errors)
}




################################
# learning curves tree
# - best selects number of terminal nodes
################################
learning_curve_tree = function(df, formula, results_column_name, number_of_steps=5, seed=13, best=NULL) {
  set.seed(seed)
  folds.0 <- df
  threshold <- 0.5
  errors=matrix(NA, number_of_steps, 2)
  folds.0$ALEATORIO <- floor(runif(dim(folds.0)[1], min=0, max=number_of_steps+2))
  
  for (step in 0:(number_of_steps-1)) {
    print(paste("step", step))
    folds.0.train <- folds.0[folds.0$ALEATORIO <= step,]
    folds.0.test <- folds.0[folds.0$ALEATORIO > step+1,]
    
    tree.Apm <- tree(formula, data=folds.0.train, split="gini")
    if (is.null(best)) {
      prune.Apm.tree <- tree.Apm
    } else {
      prune.Apm.tree = prune.misclass(tree.Apm, best=best, loss=loss)
    }
    prune.Apm.tree.preds.class <- predict(prune.Apm.tree, type="class")
    errors[step+1, 1] <- mean(prune.Apm.tree.preds.class != folds.0.train[, results_column_name])
    prune.Apm.tree.preds.class <- predict(prune.Apm.tree, type="class", newdata=folds.0.test)
    errors[step+1, 2] <- mean(prune.Apm.tree.preds.class != folds.0.test[, results_column_name])
  }
  par(mfrow=c(1,1))
  plot(errors[,1], type="l", col="blue", ylim=c(min(errors), max(errors)))
  lines(errors[,2], type="l", col="red")
  return(errors)
}

################################
# learning curves party
# - ctree_control for complexity
################################
learning_curve_party = function(df, formula, results_column_name, number_of_steps=5, seed=13, ctree_control=NULL) {
  set.seed(seed)
  folds.0 <- df
  threshold <- 0.5
  errors=matrix(NA, number_of_steps, 2)
  folds.0$ALEATORIO <- floor(runif(dim(folds.0)[1], min=0, max=number_of_steps+2))
  
  for (step in 0:(number_of_steps-1)) {
    print(paste("step", step))
    folds.0.train <- folds.0[folds.0$ALEATORIO <= step,]
    folds.0.test <- folds.0[folds.0$ALEATORIO > step+1,]
    
    if (is.null(ctree_control)) {
      party.Apm <- ctree(formula, data=folds.0.train)
    } else {
      party.Apm <- ctree(formula, data=folds.0.train, control=ctree_control)
    }
    party.Apm.preds <- predict(party.Apm)
    party.Apm.preds.conv = ifelse(party.Apm.preds > threshold, TRUE, FALSE)
    errors[step+1, 1] <- mean(party.Apm.preds.conv != folds.0.train[, results_column_name])
    party.Apm.preds <- predict(party.Apm, newdata=folds.0.test)
    party.Apm.preds.conv = ifelse(party.Apm.preds > threshold, TRUE, FALSE)
    errors[step+1, 2] <- mean(party.Apm.preds.conv != folds.0.test[, results_column_name])
  }
  par(mfrow=c(1,1))
  plot(errors[,1], type="l", col="blue", ylim=c(min(errors), max(errors)))
  lines(errors[,2], type="l", col="red")
  return(errors)
}

