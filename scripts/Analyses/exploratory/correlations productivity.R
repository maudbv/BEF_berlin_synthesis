# Correlations for productivity


## Select factors ####
data.prod <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch",
                              "SVF",
                              "TreeCover_patch",
                              "Seal_500",
                              "mean_tempNight_summer",
                              "N",
                              "P",
                              "KAK"
                            )],
                   Biodiv_data[EF_data$ID_plot,
                               c("PlantHerb_SR", #9
                                 "PlantHerb_Cover",
                                 "PlantHerb_Shannon",
                                 "NeophyteHerb_Prop",
                                 "Pollinators_SR",
                                 "Pollinators_Abun"
                               )],
                   EF_data[, c(
                     "BGB_C_total", #15
                     "BGB_C_fine",
                     "AGB_C"
                   )]
)

data.prod = na.omit(data.prod)
dim(data.prod)
data.prod$TreeCover_patch <- 100 - data.prod$TreeCover_patch
names(data.prod)[names(data.prod)=="TreeCover_patch"] <- "Opp.TreeCover_patch"
# standardize:
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
data.prod <- as.data.frame(apply(data.prod,2,stdze))

# check:
varTable(data.prod)

## Explore correlations ####

# define list of indicators: what variables are associated with # what latent variables
prod_blocks = list(Patch = 1:3,
                   urban.matrix = 4:5,
                   Soil = 6:8 ,
                   Vegetation = 9:11 ,
                   Neophytes = 12,
                   Poll.div = 13:14,
                   BG_Prod = 15:16,
                   AG_Prod = 17)
corMat <- cor(data.prod[,unlist(prod_blocks)],
              use = "pairwise.complete.obs",
              method = "pearson")

## graph
quartz()
qgraph(corMat.env, graph = "cor",
       layout = "spring",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.prod),
       groups = substr(names(unlist(prod_blocks)),1,5),
       nodeNames = colnames(corMat),
       legend.cex = 0.4,
       cut = 0, minimum = 0, maximum = 1,
       asize = 10,
       details = TRUE,
       palette = "pastel",
       theme = "Borkulo",
       title = "productivity correlations")


# Check simple pearson correlations for environment only
prod_blocks = list(Patch = 1:3,
                   urban.matrix = 4:5,
                   Soil = 6:8)

corMat.env <- cor(data.prod[,unlist(prod_blocks)],
              use = "pairwise.complete.obs",
              method = "pearson")

qgraph(corMat.env, graph = "cor",layout = "spring",
       threshold = FALSE, alpha = 0.05,groups =  substr(names(unlist(prod_blocks)),1,3))

# Check simple pearson correlations for biodiv EF only
prod_blocks = list(Vegetation = 9:11 ,
                   Neophytes = 12,
                   Poll.div = 13:14,
                   BG_Prod = 15:16,
                   AG_Prod = 17)
corMat.bef <- cor(data.prod[,unlist(prod_blocks)],
                  use = "pairwise.complete.obs",
                  method = "pearson")

qgraph(corMat.bef, graph = "cor",layout = "spring",
       threshold = FALSE, alpha = 0.05,groups =  substr(names(unlist(prod_blocks)),1,3))



