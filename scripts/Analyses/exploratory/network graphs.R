## Network graphs with igraph
library(igraph)

## Correlation network graphs exampels
library(qgraph)
library(psych)

data.poll <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch",
                              "Road_Dist",
                              "Seal_500",
                              "ShHerb_500",
                              "mean_tempDay_summer",
                              "mean_tempNight_summer")],
                   Biodiv_data[EF_data$ID_plot,
                               c("PlantHerb_SR","PlantHerb_Cover",
                                 "Grass_Cover",
                                 "Indigenous_Cover","Indigenous_SR",
                                 "NeophyteHerb_Prop",
                                 "NeophyteHerb_Cover",
                                 "Pollinators_SR",
                                 "Pollinators_Abun")],
                   Poll =  EF_data$PollinationVisitCounts
)

groups <- c(rep("Abiotic", 6),
            rep("Biodiv", 7),
            rep("Pollinator_Diversity",2),
            "Function")
names(data.poll) <- c("Size","Road_Dist","Urb","ShHerb","TempDay","tempNight","Pla_SR","Pla_C","Grass_Cov","Ind_Cover","Ind_SR","Neo_Prop","Neo_Cover","Pol_SR","Pol_A","Pol")
data.poll <- na.omit(data.poll)


corMat <- cor(data.poll,
              use = "pairwise.complete.obs",
              method = "pearson") 

## Ring layout for correlations
qgraph(corMat, graph = "cor",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.poll), 
       nodeNames = colnames(data.poll), legend.cex = 0.25,
       cut = 0, minimum = "sig", maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title = "Pollination correlations")


qgraph(corMat, graph = "pcor",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.poll), 
       nodeNames = colnames(data.poll), legend.cex = 0.25,
       groups = groups,
       cut = 0, minimum = "sig", maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title = "Pollination correlations")

## Spring layout for correlations
qgraph(corMat, graph = "cor",  layout = "spring",
       threshold = FALSE, alpha = 0.05,
       groups = groups,
       sampleSize = nrow(data.poll), 
       nodeNames = colnames(data.poll), legend.cex = 0.25,
       cut = 0, minimum = "sig", maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title = "Pollination correlations")

## Spring layout for Glasso
qgraph(corMat, graph = "glasso",  layout = "spring",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.poll), 
       nodeNames = colnames(data.poll), 
       cut = 0, minimum = 0, maximum = 1,
       esize = 10,
       details = TRUE,
       title = "Pollination correlations")


###  Correlation network for Productivity ####
library(qgraph)
library(psych)

data.prod <- cbind(Env_data[EF_data$ID_plot,
                            c("Seal_500",
                              "ShHerb_500",
                              "mean_tempDay_summer",
                              "mean_tempNight_summer",
                              "Size_Patch",
                              "Road_Dist",
                              "SVF",
                              "Wc","N")],
                   Biodiv_data[EF_data$ID_plot,
                               c("PlantHerb_SR","IndigenousHerb_SR",
                                 "IndigenousHerb_Cover",
                                 "NeophyteHerb_Prop")],
                   EF_data[, c("AGB_C","AGB_A","AGB_N",
                               "BGB_C_total","BGB_C_fine")]
)

groups <- c(rep("Urban matrix",4),
            rep("Patch quality", 5),
            rep("Native vegetation", 3),
            rep("Alien vegetation", 1),
            rep("Productivity",5))
# names(data.prod) <- c("Size","Road_Dist","Urb","ShHerb","TempDay","tempNight","Pla_SR","Pla_C","Grass_Cov","Ind_Cover","Ind_SR","Neo_Prop","Neo_Cover","Pol_SR","Pol_A","Pol")

data.prod <- na.omit(data.prod)


corMat <- cor(data.prod,
              use = "pairwise.complete.obs",
              method = "pearson") 

## Ring layout for correlations
qgraph(corMat, graph = "cor",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.prod), 
       nodeNames = colnames(data.prod),
       labels= abbreviate(names(data.prod), 8),
       legend.cex = 0.3,
       cut = 0.6, minimum = "sig", maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title = "Productivity corr. (P<0.05)")

qgraph(corMat, graph = "cor",
       threshold = FALSE, alpha = 0.05,
       groups= groups,
       sampleSize = nrow(data.prod), 
       nodeNames = colnames(data.prod),
       labels= abbreviate(names(data.prod), 5),
       legend.cex = 0.25,
       cut = 0.6, minimum = "sig", maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title =  "Productivity corr. (P<0.05)")


## Spring layout for correlations
qgraph(corMat, graph = "cor",  layout = "spring",
       threshold = FALSE, alpha = 0.05,
       groups = groups,
       sampleSize = nrow(data.prod), 
       nodeNames = names(data.prod),
       labels= abbreviate(names(data.prod), 5),
       legend.cex = 0.25,
       cut = 0, minimum = "sig" , maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title ="Productivity corr. (P<0.05)")


qgraph(corMat, graph = "pcor",  layout = "spring",
       threshold = FALSE, alpha = 0.05,
       groups = groups,
       sampleSize = nrow(data.prod), 
       nodeNames = names(data.prod),
       labels= abbreviate(names(data.prod), 5),
       legend.cex = 0.25,
       cut = 0, minimum = "sig" , maximum = 1,
       esize = 10,
       details = TRUE,
       palette = "pastel",
       title = "Productivity pcorrelations")
