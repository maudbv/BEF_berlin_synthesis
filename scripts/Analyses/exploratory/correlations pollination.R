# Correlations for pollination

# Explore pearson correlations ####


# Check simple pearson correlations
corMat <- cor(data.poll[,unlist(poll_blocks)],
              use = "pairwise.complete.obs",
              method = "pearson") 

## Ring layout for correlations
quartz()
cor_poll_graph <- qgraph(corMat, graph = "cor", layout = "spring",
                         threshold = FALSE, alpha = 0.05,
                         sampleSize = nrow(data.poll), 
                         groups = substr(names(unlist(poll_blocks)),1,5),
                         nodeNames = colnames(corMat),
                         legend.cex = 0.4,
                         cut = 0, minimum = 0, maximum = 1,
                         asize = 10,
                         details = TRUE,
                         palette = "pastel",
                         theme = "Borkulo",
                         title = "Pollination correlations")
