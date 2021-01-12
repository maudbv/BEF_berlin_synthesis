plot.plspm.boot <- function(model, multi = FALSE) {
 
   if (multi) {
     print("Comparing three different models")
    x <- model[[1]]$boot$paths
    y <- model[[2]]$boot$paths
    z <- model[[3]]$boot$paths
    
    if (is.null(names(model))) names(model) <- as.character(c(1:3))
    
    xlims <- range(c(x$perc.025,x$perc.975,
            y$perc.025,y$perc.975,
            z$perc.025,z$perc.975))
    
   paths <- unique(c(rownames(x), rownames(y), rownames(z)))
   paths.tab <- matrix(NA, nrow = length(paths),ncol = 5 )
  
   paths.tab<- data.frame(model = names(model)[1],
                          path = paths,
                          x[match(paths,rownames(x)),]
   )
   rownames(paths.tab) <- paths
   
   paths.tab<- rbind(paths.tab,
                     data.frame(model = names(model)[2],
                                path = paths,
                                y[match(paths,rownames(y)),])
   )
                     
   paths.tab<- rbind(paths.tab,
                     data.frame(model = names(model)[3],
                                path = paths,
                                z[match(paths,rownames(z)),])
   )
   
                                
                         
  } else {
    print("Only one model")
  x <- model$boot$paths
  xlims <- range(c(x$perc.025,x$perc.975))
  paths <- rownames(x)
  paths.tab<- data.frame(model = names(model)[1],
                         path = paths,
                         x
                         )
  
  }
  
  par (mar = c(4,14,2,2))
  
  for (i in 1:length(unique(paths.tab$model))) {
  tab <- paths.tab[paths.tab$model == unique(paths.tab$model)[i],]
  ys <- 1:nrow(tab) + (i - 1)/5
  if (multi) {
    cols <- c("springgreen1","darkgrey","coral")[i]
    } else { 
    cols = "darkgrey"
    }
  
  
 if (i == 1)  {
   plot(tab$Mean.Boot, ys,
        xlim = xlims,
        ylim = range(ys) + c(0,1),
        type = "n",
        xlab = "path coefficients", 
        ylab = "",
        yaxt = "n",
        bty = "n")
   
   box(col = "grey")
   abline(v = 0, col = "grey")
   axis(2, at = ys,
       labels = rownames(tab),
       cex.axis = 0.8,
       las = 1,
       lwd = 1)
 }
  segments(tab$perc.025 , ys,tab$perc.975,ys,
           col = cols)
  segments(tab$Mean.Boot - tab$Std.Error, ys,
           tab$Mean.Boot + tab$Std.Error,ys,
           col = cols, lwd = 3)
  points(tab$Mean.Boot, ys,
          pch = 20)
  points(tab$Original, ys, col = cols, pch = "x")
  points(tab$Mean.Boot, ys,pch = 20)
  }
  if(multi) {
  legend('topleft', legend =names(model),
         text.col = c("springgreen1","darkgrey","coral"),
         cex = 0.7)
  }
}   
