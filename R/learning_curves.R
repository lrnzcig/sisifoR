################################
# utilities for generating learning curves
# for models:
# - glmnet
# - tree (decision trees)
# - party (decision trees)
#
# TODO refactoring so that control is common for the 3 of them
#      (refactor de errors matrix for using lapply easily)
################################


################################
# learning curves glmnet
# - family binomial
# - applies regularization at every step,
#   selects lambda.1se
################################
learning_curve_glmnet = function(input_data, formula, results_column_name, number_of_steps=5, seed=13) {
  set.seed(seed)
  threshold <- 0.5
  errors=matrix(NA, number_of_steps, 2)
  rand <- floor(runif(dim(input_data)[1], min=0, max=number_of_steps+1))
  
  for (step in 0:(number_of_steps-1)) {
    print(paste("step", step))
    
    folds.x <- model.matrix(formula, data=input_data)
    folds.x.train <- folds.x[rand <= step,]
    folds.x.test <- folds.x[rand > step,]
    folds.y.train <- input_data[rand <= step, results_column_name]
    folds.y.test <- input_data[rand > step, results_column_name]
    
    folds.cv.lasso=cv.glmnet(folds.x.train, folds.y.train, alpha=1, family="binomial")
    folds.cv.lasso.preds.train = predict(folds.cv.lasso, folds.x.train, s=folds.cv.lasso$lambda.1se, type="response")
    folds.cv.lasso.preds.train.conv = ifelse(folds.cv.lasso.preds.train > threshold, TRUE, FALSE)
    errors[step+1, 1] <- mean(folds.cv.lasso.preds.train.conv != folds.y.train)
    folds.cv.lasso.preds.test = predict(folds.cv.lasso, folds.x.test, s=folds.cv.lasso$lambda.1se, type="response")
    folds.cv.lasso.preds.test.conv = ifelse(folds.cv.lasso.preds.test > threshold, TRUE, FALSE)
    errors[step+1, 2] <- mean(folds.cv.lasso.preds.test.conv != folds.y.test)
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
learning_curve_tree = function(input_data, tree_formula, results_column_name, number_of_steps=5, seed=13, best=NULL,
                               loss=matrix(c(0, 1, 1, 0), nrow=2, ncol=2)) {
  set.seed(seed)
  threshold = 0.5
  errors=matrix(NA, number_of_steps, 2)
  rand <- floor(runif(dim(input_data)[1], min=0, max=number_of_steps+1))

  for (step in 0:(number_of_steps-1)) {
    print(paste("step", step))
    tree.obj <- tree(tree_formula, data=input_data[rand <= step,], split="gini")
    pruned <- FALSE
    if (! is.null(best)) {
      terminal_nodes = dim(tree.obj$frame[tree.obj$frame$var == "<leaf>", ])[1]
      if (best < terminal_nodes) {
        prune.tree.obj = prune.misclass(tree.obj, best=best, loss=loss)
        pruned = TRUE
        preds.class.train = predict(prune.tree.obj, type="class", newdata=input_data[rand <= step,])
        preds.class.test = predict(prune.tree.obj, type="class", newdata=input_data[rand > step,])
      }
    }
    if (! pruned) {
      preds.class.train = predict(tree.obj, type="class", newdata=input_data[rand <= step,])
      preds.class.test = predict(tree.obj, type="class", newdata=input_data[rand > step,])
    }
    errors[step+1, 1] = mean(preds.class.train != input_data[rand <= step, results_column_name])
    errors[step+1, 2] = mean(preds.class.test != input_data[rand > step, results_column_name])
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
learning_curve_party = function(input_data, formula, results_column_name, number_of_steps=5, seed=13, ctree_control=NULL) {
  set.seed(seed)
  threshold <- 0.5
  errors=matrix(NA, number_of_steps, 2)
  rand <- floor(runif(dim(input_data)[1], min=0, max=number_of_steps+1))
  
  for (step in 0:(number_of_steps-1)) {
    print(paste("step", step))
    folds_train <- input_data[rand <= step,]
    folds_test <- input_data[rand > step,]
    
    if (is.null(ctree_control)) {
      party.tree <- ctree(formula, data=folds_train)
    } else {
      party.tree <- ctree(formula, data=folds_train, control=ctree_control)
    }
    party.tree.preds <- predict(party.tree)
    party.tree.preds.conv = ifelse(party.tree.preds > threshold, TRUE, FALSE)
    errors[step+1, 1] <- mean(party.tree.preds.conv != folds_train[, results_column_name])
    party.tree.preds <- predict(party.tree, newdata=folds_test)
    party.tree.preds.conv = ifelse(party.tree.preds > threshold, TRUE, FALSE)
    errors[step+1, 2] <- mean(party.tree.preds.conv != folds_test[, results_column_name])
  }
  par(mfrow=c(1,1))
  plot(errors[,1], type="l", col="blue", ylim=c(min(errors), max(errors)))
  lines(errors[,2], type="l", col="red")
  return(errors)
}

