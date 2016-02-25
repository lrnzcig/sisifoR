################################
# utilities for generating business plots 
# for classification problems
################################


################################
# simple business plot, not including costs
# from predictions and results
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
  if (is.factor(r$p)) {
    # if prediction is a factor, order alphabetically
    r$p <- as.character(r$p)
    r <- r[order(r$p),]        
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
  percent.r <- runSum(tp, window_size) / runSum(tp + fp, window_size)
  percent.r <- c(0, percent.r)
  # plots
  if (is.null(xlim)) {
    number_of_x <- max(length(bi.x) * show_x_proportion, 500)
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
  legend("topright", max(percent.baseline, percent.r[0:number_of_x]), c("classifier", "random"), cex=0.8,
         col=c("red", "blue"), lty=1)
}
