# Correlations for productivity


## Select factors ####
data.decomp <- cbind(Env_data[EF_data$ID_plot,
                              c("pH",
                                "P",
                                "C",
                                "N",
                                "KAK",
                                "Seal_500",
                                "mean_temp_summer",
                                "HanskiHist",
                                "Cover_total",
                                "Cover_litter"
                              )],
                     Biodiv_data[EF_data$ID_plot,
                                 c("Plant_SR",
                                   "NeophyteHerb_Prop",
                                   "Neophyte_RelCover",
                                   "BG_Decomposer_Abun",
                                   "Decomposer_Abun",
                                   "BG_Decomposer_TR",
                                   "Decomposer_SR"
                                 )],
                     EF_data[, c("decomposition_s",
                                 "decomposition_k"
                     )]
)

data.decomp = na.omit(data.decomp)
dim(data.decomp)
# data.decomp$decomposition_s <- - data.decomp$decomposition_s

# # standardize:
 stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
 data.decomp <- as.data.frame(apply(data.decomp,2,stdze))

## Explore correlations ####

# define list of indicators: what variables are associated with # what latent variables
decomp_blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
                     Soil = c("pH", "P","N","KAK"),
                     History = c("HanskiHist"),
                     Vegetation = c("Cover_total","Cover_litter","Plant_SR"),
                     D_Div  = c("BG_Decomposer_TR","Decomposer_SR"),
                     D_Abun  = c("BG_Decomposer_Abun","Decomposer_Abun"),
                     D_Rate = c("decomposition_k")
)
 
corMat <- cor(data.decomp[,as.vector(unlist(decomp_blocks))],
              use = "pairwise.complete.obs",
              method = "pearson")

## graph
quartz()
qgraph(corMat, graph = "cor",
       layout = "spring",
       threshold = "sig", alpha = 0.05,
       sampleSize = nrow(data.decomp),
       groups = substr(names(unlist(decomp_blocks)),1,5),
       nodeNames = colnames(corMat),
       legend.cex = 0.4,
       cut = 0, minimum = 0, maximum = 1,
       asize = 10,
       details = TRUE,
       palette = "pastel",
       theme = "Borkulo",
       title = "Decomposition correlations")


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



