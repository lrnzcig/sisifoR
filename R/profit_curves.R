################################
# profit curves for business evaluation
#
# Reference: F.Provost & T.Fawcett. Data Science for Business. O'Really 2013.
################################

cummulative_response <- function(preds,
                                 input_data,
                                 results_column_name,
                                 x_axis_ratio=FALSE,
                                 y_axis_ratio=TRUE,
                                 plotit=TRUE) {
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  q.curve.real <- get_cumm_curve_values(ordered_data, results_column_name)
  
  if (plotit == TRUE) {
    x_max <- length(q.curve.real)
    x_seq <- 0:x_max
    if (y_axis_ratio) {
      q.curve.real <- q.curve.real / length(q.curve.real)
    }
    if (x_axis_ratio) {
      x_seq <- x_seq / length(q.curve.real)
      x_max <- x_max / length(q.curve.real)
    }
    xlab = paste("customers", ifelse(x_axis_ratio == TRUE, "(ratio)", "(total)"))
    ylab = paste("conversions", ifelse(y_axis_ratio == TRUE, "(ratio of total customers)", "(total)"))
    plot(x_seq, c(0, q.curve.real), type="l", col="blue", xlab=xlab, ylab=ylab)
    lines(c(0,x_max), c(0, q.curve.real[length(q.curve.real)]), col="red")
  }
  # area of the square with heigh equals to the the number of conversions
  square_area <- length(q.curve.real)*q.curve.real[length(q.curve.real)]
  # Simpson's rule for area
  return((sum(diff(1:length(q.curve.real))*rollmean(q.curve.real, 2))-square_area/2)/square_area)
}

# support function: calculates values of the curve
get_cumm_curve_values <- function(ordered_data,
                                  results_column_name) {
  q.plus <- cumsum(ifelse(ordered_data[,results_column_name] == 1, 1, 0))
  return(q.plus)
}
