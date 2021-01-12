# Trying out PLSPM

library(plspm)

## Pollinators
data.poll <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch",
                              "SVF",
                                "Seal_500",
                                "mean_tempNight_summer",
                              "Hanski3D_DryGr",
                              "ShDry_500"
                              )],
                   Biodiv_data[EF_data$ID_plot,
                               c("Plants_insect.poll_SR",
                                 "Plants_insect.poll_Cover",
                                 "Neophyte_Prop",
                                 "Wildbees_polylectic_SR"  ,
                                 "Pollinators_SR",
                                 "Pollinators_Abun"
                                )],
                  Poll =  EF_data$PollinationVisitCounts
)

data.poll = na.omit(data.poll)
data.poll$NeophyteHerb_Prop = 1 - data.poll$NeophyteHerb_Prop  
dim(data.poll)

        
# define list of indicators: what variables are associated with # what latent variables
poll_blocks = list(Patch.size = 1:2,
                   urban.matrix = 3:6,
                   Vegetation = 7:9 ,
                   Poll.div = 10:11,
                   Pollination = 12:13 )


# rows of the inner model matrix
patch.size =    c(0, 0, 0, 0, 0)
urban.matrix =  c(0, 0, 0, 0, 0)
Vegetation =    c(1, 1, 0, 0, 0)
Poll.div =      c(1, 1, 1, 0, 0)
Poll =          c(1, 1, 1, 1, 0)

# path matrix created by row binding
# poll_path = rbind(urban.matrix, patch.size, Neophytes, Vegetation,  Poll )
poll_path = rbind( patch.size, urban.matrix, Vegetation, Poll.div, Poll )

# add column names (optional)
colnames(poll_path) = rownames(poll_path)
innerplot(poll_path)


# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes = c("A", "B", "A", "A" , "A")                              
          
# Run PLSPM analysis                      
poll_pls = plspm(data.poll, poll_path,
                 poll_blocks,
                 modes = poll_modes,
                 scaled = TRUE,
                 boot.val = TRUE,
                 br = 200
                )

# Plot output of PLSPM

plot(poll_pls,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls$path_coefs)) )

plot(poll_pls, what="loadings")
 
poll_pls$unidim
poll_pls$inner_model
poll_pls$inner_summary
poll_pls$path_coefs
poll_pls$gof #45 %
poll_pls$boot$paths

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

quartz()
qgraph(corMat, graph = "cor", layout = "spring",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.poll), 
       groups = substr(names(unlist(poll_blocks)),1,5),
       nodeNames = colnames(corMat),
       legend.cex = 0.4,
       cut = 0, minimum = "sig", maximum = 1,
       asize = 10,
       details = TRUE,
       palette = "pastel",
       theme = "Borkulo",
       title = "Pollination correlations")

# qgraph(corMat, graph = "pcor", layout = cor_poll_graph$layout,
#                          threshold = FALSE, alpha = 0.05,
#                          sampleSize = nrow(data.poll), 
#                          groups = substr(names(unlist(poll_blocks)),1,5),
#                          nodeNames = colnames(corMat),
#                          legend.cex = 0.4,
#                          cut = 0, minimum = "sig", maximum = 1,
#                          asize = 10,
#                          details = TRUE,
#                          palette = "pastel",
#                          theme = "Borkulo",
#                          title = "Pollination partial correlations")



## Second version  poll_pls2

#rows of the inner model matrix
urban.matrix =  c(0, 0, 0, 0)
patch.size =    c(0, 0, 0, 0)
Neophytes =     c(1, 1, 0, 0)
Poll =          c(1, 1, 1, 0)

# path matrix created by row binding
# poll_path = rbind(urban.matrix, patch.size, Neophytes, Vegetation,  Poll )
poll_path = rbind(urban.matrix, patch.size, Neophytes,Poll )

# add column names (optional)
colnames(poll_path) = rownames(poll_path)
innerplot(poll_path)

# define list of indicators: what variables are associated with # what latent variables
poll_blocks = list(urban.matrix = c(2,3,6),
                   Patch.size = 1,
                   Neophytes = 12:13,
                   # Vegetation = 7:9 ,
                   Poll = 14:16 )

# Success in formative mode B
poll_modes = c("A", "A", "A",  "A")                       
poll_pls2 = plspm(data.poll, poll_path,
                 poll_blocks,
                 modes = poll_modes,
                 scaled = TRUE,
                 boot.val = TRUE,br = 500
)


plot(poll_pls2,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls2$path_coefs)))


outerplot(poll_pls2)     

poll_pls2$unidim
poll_pls2$inner_model
poll_pls2$inner_summary
poll_pls2$path_coefs
poll_pls2$gof
poll_pls2$boot$paths

