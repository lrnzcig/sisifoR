require(zoo)

#
# get qini value for all lambdas in s_range
# - model_object: fitted model to predict the data
# - newdata: data for the predictions
# - s_range: values for lambda
# - results_column_name: target; default value is ok when using uplift::rvtu
#
qini_for_lambda_range <- function(model_object,
                                  newdata,
                                  s_range,
                                  input_data,
                                  plotit=FALSE) {
  preds.z.interactions <- predict(model_object, newdata,
                                  s=s_range, type="response")
  if (length(s_range) > 1 & plotit == TRUE) {
    warning("Vector of lambdas provided, the qini curves will not be plotted")
    plotit=FALSE
  }
  qini_per_lambda <- sapply(1:(length(s_range)), function (index) {
    qini_from_data(preds.z.interactions[,index], input_data, plotit=plotit)
  })
  return(qini_per_lambda)
}

#
# get qini value and plot it from input data and vector of predictions
# - columns names by default are ok if using uplift::rvtu
# - ideal_order_column_name & treat_column_name are used for ordering the ideal response
# - input data is ordered by predictions only
# - results_column_name & treat_column_name are used to sum/penalize each conversion
# - by default does not plot
#
qini_from_data <- function(preds,
                           input_data,
                           ideal_order_column_name="z",
                           results_column_name="y",
                           treat_column_name="ct",
                           plotit=FALSE) {
  # ideal
  ordered_data <- input_data[order(input_data[,ideal_order_column_name], input_data[,treat_column_name], decreasing=TRUE),]
  q.curve.ideal <- get_qini_curve_values(ordered_data, results_column_name, treat_column_name)

  if (q.curve.ideal[length(q.curve.ideal)] == 0) {
    warning("O conversions in the data. The function returns 0 but actually it should be 'unknown'")
    return(0)
  }
  
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  q.curve.real <- get_qini_curve_values(ordered_data, results_column_name, treat_column_name)
  
  if (length(q.curve.real) != length(q.curve.ideal)) {
    stop("Check that length of input data corresponds to length of predictions")
  }
  
  if (plotit == TRUE) {
    plot(0:length(q.curve.ideal), c(0, q.curve.ideal), type="l", col="black", xlab="customers", ylab="conversions")
    points(0:length(q.curve.ideal), c(0, q.curve.real), type="l", col="blue")
    lines(c(0,length(q.curve.ideal)), c(0, q.curve.ideal[length(q.curve.ideal)]), col="red")
  }
  # area of the square with heigh equals to the the number of conversions
  square_area <- length(q.curve.ideal)*q.curve.ideal[length(q.curve.ideal)]
  # Simpson's rule for area
  return((sum(diff(1:length(q.curve.real))*rollmean(q.curve.real, 2))-square_area/2)/square_area)
}

# support function: calculates values of the curve, penalizing if control element converts
get_qini_curve_values <- function(ordered_data,
                                  results_column_name="y",
                                  treat_column_name="ct") {
  q.plus <- cumsum(ifelse(ordered_data[,treat_column_name] == 1 & ordered_data[,results_column_name] == 1, 1, 0))
  q.minus <- cumsum(ifelse(ordered_data[,treat_column_name] == 0 & ordered_data[,results_column_name] == 1, 1, 0))
  q.interactions <- q.plus - q.minus
  return(q.interactions)
}

