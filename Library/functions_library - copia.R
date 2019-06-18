


my_summary <- function(thedata){
  res = apply(thedata, 2, function(r) c(min(r), quantile(r, 0.25), quantile(r, 0.5), mean(r), quantile(r, 0.75), max(r), sd(r)))
  res <- round(res,2)
  colnames(res) <- colnames(thedata)
  rownames(res) <- c("min", "25 percent", "median", "mean", "75 percent", "max", "std")
  t(res)
}


hist_den = function(redwine, x_string,graph_title) {
  hist(redwine[,i], # histogram 
       col="peachpuff", # column color
       border="black",
       prob = FALSE, # show densities instead of frequencies
       xlab = x_string,
       main = graph_title)
  #  lines(density(redwine[,i]), # density plot
  #       lwd = 2, # thickness of line
  #       col = "chocolate3")
  
  #ggplot(redwine, aes_string(x = x_string))+ # the histogram will display "density" on its y-axis
  #                 geom_histogram(aes(y = ..density..), binwidth = 0.1, colour = "blue", fill = "white")+
  #                geom_density(alpha = .2, fill="#FF6655")
  
}
