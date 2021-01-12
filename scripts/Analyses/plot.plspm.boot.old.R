plot.plspm.boot <- function(model) {
  x <- model$boot$paths
  par (mar = c(4,10,2,2))
  plot(x$Mean.Boot, 1:nrow(x),
       xlim = range(c(x$perc.025,x$perc.975)),
       type = "n",
       xlab = "path coefficients", 
       ylab = "",
       yaxt = "n",
       bty = "n")
  box(col = "grey")
  abline(v = 0, col = "grey")
  axis(2, at = 1:nrow(x),
       labels = rownames(x),cex.axis = 0.8,
       las = 1,
       lwd =0)
  segments(x$perc.025 , 1:nrow(x),x$perc.975,1:nrow(x),
           col = "darkgrey")
  segments(x$Mean.Boot - x$Std.Error, 1:nrow(x),
           x$Mean.Boot + x$Std.Error,1:nrow(x),
           col = "darkgrey", lwd = 3)
  points(x$Mean.Boot, 1:nrow(x),
          pch = 20)
  points(x$Original, 1:nrow(x), col = "darkgrey", pch = "x")
  points(x$Mean.Boot, 1:nrow(x),pch = 20)
  
  }   