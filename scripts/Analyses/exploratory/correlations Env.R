
## Select factors
data.env <- cbind(Env_data[EF_data$ID_plot,
                              c("Size_Patch",
                                "SVF",
                                "TreeCover_patch",
                                "C_N",
                                "N",
                                "P",
                                "Wc",
                                "metals",
                                "Seal_500",
                                "mean_tempNight_summer",
                                "Hanski3D_DryGr",
                                "HanskiHist",
                                "ShDry_500",
                                "ShGrass_500",
                                "BioRich_500",
                                "Cover_total"
                              )],
                     Biodiv_data[EF_data$ID_plot,
                                 c("PlantHerb_SR",
                                   "NeophyteHerb_Prop"
                                 )]
                 
)

data.env = na.omit(data.env)
dim(data.env)

# define list of indicators: what variables are associated with # what latent variables
env_blocks = list(Patch = 1:3,
                     Soil = 4:8,
                     urban.matrix = 9:10,
                  Connectivity = 11:15,
                  Vegetation = 16:17,
                  Neophytes = 18
                  )

# Check simple pearson correlations
corMat <- cor(data.env[,unlist(env_blocks)],
              use = "pairwise.complete.obs",
              method = "pearson") 

## Ring layout for correlations
quartz()
qgraph(corMat, graph = "cor",
       layout = "spring",
       threshold = FALSE, alpha = 0.05,
       sampleSize = nrow(data.env), 
       groups = substr(names(unlist(env_blocks)),1,4),
       nodeNames = colnames(corMat),
       legend.cex = 0.4,
       cut = 0, minimum = 0.2, maximum = 1,
       asize = 10,
       details = TRUE,
       palette = "pastel",
       theme = "Borkulo",
       title = "environmental correlations")
