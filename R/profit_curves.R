################################
# profit curves for business evaluation
#
# Reference: F.Provost & T.Fawcett. Data Science for Business. O'Really 2013.
################################
require(zoo)

# cummulative response: just sum of conversions over customers, ordered by the predictions
cumulative_response <- function(preds,
                                input_data,
                                results_column_name,
                                main_title=NULL,
                                x_axis_ratio=FALSE,
                                y_axis_ratio=TRUE,
                                print_auc=TRUE,
                                plotit=TRUE) {
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  q.curve.real <- get_cum_curve_values(ordered_data, results_column_name)
  return(base_curve(q.curve.real, main_title, x_axis_ratio, y_axis_ratio, print_auc, plotit))
}

# similar to cumulative response, but penalizing as in qini curve
# (conversions in the control data get a -1)
cum_penalized_curve <- function(preds,
                                input_data,
                                results_column_name,
                                treat_column_name,
                                main_title=NULL,
                                x_axis_ratio=FALSE,
                                y_axis_ratio=TRUE,
                                print_auc=TRUE,
                                plotit=TRUE) {
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  q.curve.real <- get_qini_curve_values(ordered_data, results_column_name, treat_column_name)
  return(base_curve(q.curve.real, main_title, x_axis_ratio, y_axis_ratio, print_auc, plotit))
}

# accumulated profits taking into account profit of true positive and
# profit of false positive (i.e. -cost)
profit_curve <- function(preds,
                         input_data,
                         results_column_name,
                         profit.tp,
                         profit.fp,
                         main_title=NULL,
                         x_axis_ratio=TRUE,
                         print_auc=TRUE,
                         plotit=TRUE) {
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  q.curve.real <- get_profit_curve_values(ordered_data, results_column_name, profit.tp, profit.fp)
  return(base_curve(q.curve.real,
                    main_title, x_axis_ratio, TRUE, print_auc, plotit,
                    alt_ylab="accumulated benefit per targeted customer"))
}

# same as above, but adding a targeting cost
# (thus penalizing when targeting to customers in control group who would have converted anyway)
profit_penalized_curve <- function(preds,
                                   input_data,
                                   results_column_name,
                                   treat_column_name,
                                   profit.tp,
                                   targeting.cost,
                                   profit.fp,
                                   main_title=NULL,
                                   x_axis_ratio=TRUE,
                                   print_auc=TRUE,
                                   plotit=TRUE) {
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  # profits from the targeted customers (i.e. per value of x axis)
  q.curve.real <- get_profit_curve_values(ordered_data, results_column_name, profit.tp - targeting.cost, profit.fp)
  # profits from customers in the control group (i.e. remaining customers in target group that convert per value of x axis)
  q.control.profit <- get_profit_curve_control_values(ordered_data, results_column_name, treat_column_name, profit.tp)
  return(base_curve(q.curve.real + q.control.profit,
                    main_title, x_axis_ratio, TRUE, print_auc, plotit, 
                    alt_ylab="accumulated benefit per targeted customer",
                    add_curve=q.control.profit / length(q.curve.real)))
}

# TODO: use objects in R
base_curve <- function(q.curve.real,
                       main_title=NULL,
                       x_axis_ratio=FALSE,
                       y_axis_ratio=TRUE,
                       print_auc=TRUE,
                       plotit=TRUE,
                       alt_xlab=NULL,
                       alt_ylab=NULL,
                       add_curve=NULL) {
  # calculate area under the curve
  # area of the square with heigh equals to the the number of conversions
  square_area <- abs(length(q.curve.real)*q.curve.real[length(q.curve.real)])
  # Simpson's rule for area
  result <- (sum(diff(1:length(q.curve.real))*rollmean(q.curve.real, 2))-square_area/2)/square_area
  
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
    if (! is.null(alt_xlab)) {
      xlab = alt_xlab 
    } else {
      xlab = paste("customers", ifelse(x_axis_ratio == TRUE, "(ratio)", "(total)"))
    }
    if (! is.null(alt_ylab)) {
      ylab = alt_ylab 
    } else {
      ylab = paste("conversions", ifelse(y_axis_ratio == TRUE, "(ratio of total customers)", "(total)"))
    }
    plot(x_seq, c(0, q.curve.real), type="l", col="blue", xlab=xlab, ylab=ylab)
    lines(c(0,x_max), c(0, q.curve.real[length(q.curve.real)]), col="red")
    if (! is.null(add_curve)) {
      lines(x_seq, c(0, add_curve), type="l", col="black", lty=2)
    }
    if (! is.null(main_title)) {
      title(main_title)
    }
    if (print_auc == TRUE) {
      legend("bottomright", paste("area of uplift", 
                                  round(result, 4), 
                                  "                "), cex=0.75)
      legend("bottom", paste("maximum y", 
                             round(max(q.curve.real), 2), 
                             " for x ",
                             round(x_seq[which(q.curve.real == max(q.curve.real))], 2),
                             "                "), cex=0.75)
    }
  }
  return(result)
}

# support function: calculate values of the curve
get_cum_curve_values <- function(ordered_data,
                                 results_column_name) {
  q.plus <- cumsum(ifelse(ordered_data[,results_column_name] == 1, 1, 0))
  return(q.plus)
}

get_profit_curve_values <- function(ordered_data,
                                    results_column_name,
                                    profit.tp,
                                    profit.fp) {
  q.tp <- cumsum(ifelse(ordered_data[,results_column_name] == 1, profit.tp, 0))
  q.fp <- cumsum(ifelse(ordered_data[,results_column_name] == 0, profit.fp, 0))
  return(q.tp + q.fp)
}

get_profit_curve_control_values <- function(ordered_data,
                                            results_column_name,
                                            treat_column_name,
                                            profit.tp) {
  reversed_data = ordered_data[nrow(ordered_data):1, ]
  return(rev(cumsum(ifelse(reversed_data[, results_column_name] == 1 &
                             reversed_data[, treat_column_name] == 0, profit.tp, 0))))
}


