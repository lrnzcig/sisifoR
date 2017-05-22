plot_4series <- function(list,
                         name1, name2, name3, name4,
                         cluster_name,
                         ylim=c(75,100)) {
  par(mfrow=c(4,1))
  plot(list[[name1]], type="l", main=cluster_name,
       xlab=name1, ylim=ylim)
  if (name2 != "") {
    plot(list[[name2]], type="l",
         xlab=name2, ylim=ylim)
  }
  if (name3 != "") {
    plot(list[[name3]], type="l",
         xlab=name3, ylim=ylim)
  }
  if (name4 != "") {
    plot(list[[name4]], type="l",
         xlab=name4, ylim=ylim)
  }
}

browse_spectral_clustering <- function(cluster_object, list_of_series,
                                       ylim=c(75,100)) {
  i <- 0
  n1 <- ""
  n2 <- ""
  n3 <- ""
  n4 <- ""
  for (order in rev(cluster_object$order)) {
    if (i %% 4 == 0) n1 <- names(list_of_series)[[order]]
    if (i %% 4 == 1) n2 <- names(list_of_series)[[order]]
    if (i %% 4 == 2) n3 <- names(list_of_series)[[order]]
    if (i %% 4 == 3) {
      n4 <- names(list_of_series)[[order]]
      plot_4series(list_of_series,
                   n1,
                   n2,
                   n3,
                   n4,
                   paste("spectral", (i+1)/4),
                   ylim=ylim)
      invisible(readline(prompt="Press [enter] to continue"))
      n1 <- ""
      n2 <- ""
      n3 <- ""
      n4 <- ""
    }
    i <- i + 1
  }
  if (n3 != "" | n2 != "" | n1 != "") {
    plot_4series(list_of_series,
                 n1,
                 n2,
                 n3,
                 n4,
                 paste("spectral", ceiling((i+1)/4)),
                 ylim=ylim)
  }
}

browse_cutree_clustering <- function(cluster_object, list_of_series, height,
                                     ylim=c(75,100)) {
  cutted <- cutree(cluster_object, h=height)
  nb_of_clusters <- max(cutted)
  for (cluster in 1:nb_of_clusters) {
    i <- 0
    n1 <- ""
    n2 <- ""
    n3 <- ""
    n4 <- ""
    for (name in names(cutted)[cutted == cluster]) {
      if (i %% 4 == 0) n1 <- name
      if (i %% 4 == 1) n2 <- name
      if (i %% 4 == 2) n3 <- name
      if (i %% 4 == 3) {
        n4 <- name
        plot_4series(list_of_series,
                     n1,
                     n2,
                     n3,
                     n4,
                     paste("cluster", cluster),
                     ylim=ylim)
        invisible(readline(prompt="Press [enter] to continue"))
        n1 <- ""
        n2 <- ""
        n3 <- ""
        n4 <- ""
      }
      i <- i + 1
    }
    if (n3 != "" | n2 != "" | n1 != "") {
      plot_4series(list_of_series,
                   n1,
                   n2,
                   n3,
                   n4,
                   paste("cluster", cluster),
                   ylim=ylim)
      invisible(readline(prompt="Press [enter] to continue"))
    }
  }
}
