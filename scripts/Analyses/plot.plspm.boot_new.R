plot.plspm.boot <- function(model, multi = FALSE) {
 
   if (multi) {
     print(paste("Comparing",length(model),"different models"))
    
     path.list <- list()
     for (i in 1:length(model)) {
    path.list[[i]] <- model[[i]]$boot$paths
     }
    
    if (is.null(names(model))) names(model) <- as.character(1:length(model))
    
  xlims <- range(lapply(path.list, function(x) c(x$perc.025,x$perc.975)))
   paths <- unique(unlist(lapply(path.list, function(x) rownames(x))))
  
    paths.tab <- matrix(NA, nrow = length(paths),ncol = 5 )
   paths.tab <- sapply(1:length(model), function(i) {
     x <- path.list[[i]]
   tab<- data.frame(model = names(model)[i],
                          path = paths,
                          x[match(paths,rownames(x)),]
   )
   rownames(tab) <- paths
   return(tab)
   }, simplify = FALSE)
   paths.tab <- do.call(rbind,paths.tab)
   
  } else {
    print("Only one model")
  x <- model$boot$paths
  xlims <- range(c(x$perc.025,x$perc.975))
  paths <- rownames(x)
  paths.tab<- data.frame(model = names(model)[1],
                         path = paths,
                         x)
  
  }
  
  # Graph:
  par (mar = c(4,14,2,2))
  
  # Color scheme: 
  if (multi) { col.mods <- inlmisc::GetColors(length(model),
                                              scheme = 'smooth rainbow',
                                              stops =c(0.2,0.8) ) }
  
  for (i in 1:length(unique(paths.tab$model))) {
  tab <- paths.tab[paths.tab$model == unique(paths.tab$model)[i],]
  ys <- 1:nrow(tab) + (i - 1)/8
  if (multi) {
    cols <- col.mods[i]
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
  segments(tab$perc.025 , ys,tab$perc.975, ys,
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
  legend('topleft', legend =names(model)[length(model):1],
         text.col = col.mods[length(model):1], bty = "n", bg = "n",
         cex = 0.7)
  }
}   
