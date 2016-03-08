################################
# utilities for generating business plots 
# for classification problems
################################


################################
# simple business plot, not including costs
# from predictions and results
#
# TODO this is similar to "lift" curve, benefits:
# - directly interpretable
# - x axis is number of instances
# - some costs can be added easily?
################################
business_plot_simple = function(preds, y,
                                # additional label at the title of the graph
                                additional_label=NULL,
                                # proportion of the x values shown
                                show_x_proportion=0.1,
                                # for smoothing the curve
                                window_size=1000,
                                # if infomed, set limits to x & y axis
                                xlim=NULL, ylim=NULL) {
  number_of_customers <- length(y)
  prior.fp <- length(y[y == FALSE])
  prior.tp <- length(y[y == TRUE])
  number_of_customers == prior.fp + prior.tp
  number_of_customers == length(preds)
  # x 
  bi.x <- seq(0, number_of_customers, by=1)
  # baseline
  percent.baseline <- prior.tp / number_of_customers
  # resultados
  r <- data.frame(preds, y)
  colnames(r) <- c("p", "y")
  predictor_is_factor <- FALSE
  if (is.factor(r$p)) {
    # if prediction is a factor, order alphabetically
    r$p <- as.character(r$p)
    r <- r[order(r$p),]        
    predictor_is_factor <- TRUE
  } else {
    # else it is a probability (numeric) is assumed
    r <- r[order(-r$p),]    
  }
  tp <- r$y
  fp <- ! r$y
  # smoothing
  runSum <- function(x, window_size) {
    cumsum(x) - c(rep(0, window_size), cumsum(x[1:(length(x) - window_size)]))
  }
  if (predictor_is_factor == FALSE) {
    percent.r <- runSum(tp, window_size) / runSum(tp + fp, window_size)
  } else {
    percent.r <- NULL
    previous_window_init <- 1
    for (level in sort(levels(as.factor(r$p)))) {
      window_size_level <- length(preds[preds == level])
      current_window_end <- previous_window_init + window_size_level - 1
      print(current_window_end)
      percent.r.level <- sum(tp[previous_window_init:current_window_end]) / window_size_level
      print(percent.r.level)
      previous_window_init <- current_window_end + 1
      if (is.null(percent.r)) {
        percent.r <- rep(percent.r.level, window_size_level)
      } else {
        percent.r <- c(percent.r, rep(percent.r.level, window_size_level))
      }
    }
  }
  percent.r <- c(0, percent.r)
  print(length(percent.r))
  # plots
  if (is.null(xlim)) {
    print(length(bi.x) * show_x_proportion)
    number_of_x <- max(length(bi.x) * show_x_proportion, 500)
    print(max(percent.baseline, percent.r[0:number_of_x]))
    plot(bi.x[0:number_of_x], rep(percent.baseline, number_of_x), type="l", col="blue",
         xlab="", ylab="", ylim=c(0, max(percent.baseline, percent.r[0:number_of_x])))
    lines(bi.x[0:number_of_x], percent.r[0:number_of_x], type="l", col="red",
          xlab="", ylab="")
  } else {
    number_of_x <- xlim[2]
    plot(bi.x, rep(percent.baseline, length(bi.x)), type="l", col="blue",
         xlab="", ylab="", xlim=xlim, ylim=ylim)
    lines(bi.x, percent.r, type="l", col="red",
          xlab="", ylab="", xlim=xlim, ylim=ylim)
    
  }
  tit <- "Business evaluation"
  tit <- paste(tit, additional_label, " ")
  title(main=tit, font.main=4)
  title(xlab="number of customers")
  title(ylab="% conversion")
  legend("topright", max(percent.baseline, percent.r[0:number_of_x]), c("classifier     ", "random"), cex=0.8,
         col=c("red", "blue"), lty=1)
}

################################
# business plot including costs
# from predictions and results
#
# parameters:
# - results: predictions as probabilities
# - y.conv: real results (converted to TRUE/FALSE)
# - cost of true positive (usually positive)
# - cost of false positive (usually negative)
# - prior probability of positive
#
# Reference: F.Provost & T.Fawcett. Data Science for Business. O'Really 2013.
################################
business_plot_costs = function(results, y.conv, cost.tp=10, cost.fp=-10,
                               prior.pr=0.15) {
  # random reference line
  bi.x <- seq(0, 100, by=1)
  ucost.random <- cost.tp * prior.pr + cost.fp * (1 - prior.pr)
  cost.random <- ucost.random * bi.x
  # results line
  r <- data.frame(results, y.conv)
  colnames(r) <- c("p", "y")
  r <- r[order(-r$p),]
  tp <- r$y
  fp <- ! r$y
  # this is cost per customer who has been targeted
  cost.r <- cost.tp * tp + cost.fp * fp
  # cummulative
  cost.r <- cumsum(cost.r)
  # now to scale 0% -> 100%
  cost.r.scaled = rep(NA, length(bi.x))
  factor.y = 100 / length(cost.r)
  factor.x = trunc(1/factor.y)
  for (i in 1:length(bi.x)) {
    cost.r.scaled[i] = cost.r[(i-1)*factor.x + 1] * factor.y
    # correct last values
    if (cost.r.scaled[i] < cost.random[i]) {
      cost.r.scaled[i] = cost.random[i]
    }
  }
  plot(bi.x, cost.r.scaled, type="l", col="blue",
       xlab="", ylab="")
  # plots
  lines(bi.x, cost.random, type="l", col="red",
        xlab="", ylab="")
  title(main="Business evaluation of classifier", font.main=4)
  title(xlab="% of customers")
  title(ylab="revenue")
  legend("topright", c("classifier     ", "random"), cex=0.8,
         col=c("blue", "red"), lty=1)
  
}


