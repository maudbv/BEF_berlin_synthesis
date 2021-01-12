# PLSPM to explain grassland productivity (Biomass)

library(plspm)

## Select factors ####
data.prod <- cbind(Env_data[EF_data$ID_plot,
                            c("log_Size_Patch",
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
                           "AGB_C",
                           "AGB_G",
                           "AGB_F",
                           "AGB_L"
                           )]
)

data.prod = na.omit(data.prod)
dim(data.prod)
data.prod$TreeCover_patch <- 100 - data.prod$TreeCover_patch
names(data.prod)[names(data.prod)=="TreeCover_patch"] <- "Opp.TreeCover_patch"

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



# PLSPM 1: ALL selected variable types ####

# define Latent variables
prod_blocks = list(Patch = 1:2,
                   urban.matrix = 4:5,
                   soil = 6:7,
                   Plant.div = 9,
                   BG_Prod = 13,
                   AG_Prod = 15
                   )

# rows of the inner model matrix
prod_path = rbind( 
        Patch =        c(0,0,0,0,0,0),
        urban.matrix = c(0,0,0,0,0,0),
        soil =         c(0,0,0,0,0,0),
        Plant.div =    c(1,1,1,0,0,0),
        BG_Prod =      c(1,1,1,1,0,0),
        AG_Prod =      c(1,1,1,1,0,0)
)

# add column names (optional)
colnames(prod_path) = rownames(prod_path)
innerplot(prod_path)

# Check blocks
lapply(prod_blocks, function(x) names(data.prod)[x])

# Modes (A = reflective)
prod_modes = c("A","A","B","A", "A", "A")                              
# Run PLSPM analysis                      
prod_pls_all = plspm(data.prod, prod_path,
                 prod_blocks,
                 modes = prod_modes,
                 scaled = TRUE,
                 boot.val = TRUE,
                 br = 2000
)

prod_pls_all$unidim
prod_pls_all$inner_model
prod_pls_all$inner_summary
prod_pls_all$path_coefs
prod_pls_all$gof # 57%
prod_pls_all$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(prod_pls_all,
     arr.pos = 0.35, 
     arr.lwd = abs(10*(prod_pls_all$path_coefs)) )
title("PLSPM productivity\nall groups", adj = 0)
mtext(3, text = paste("GOF=", round(prod_pls_all$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(2,
      text = paste(
              "R2(AG Prod)=",
              round(prod_pls_all$inner_summary["AG_Prod",2],2),
              "\nR2(BG Prod)=",
              round(prod_pls_all$inner_summary["BG_Prod",2],2),
              "\nR2(Plant Div)=",
              round(prod_pls_all$inner_summary["Plant.div",2],2)
              ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 4)

quartz()
plot(prod_pls_all, what="loadings",arr.tcol = "black")
mtext(2,text = "PLSPM productivity\nall groups", las = 0, line= 1.3)


# PLSPM 2: Only environmental ####

# define Latent variables
prod_blocks = list(Patch = 1:2,
                   urban.matrix = 4:5,
                   soil = 6:7,
                   BG_Prod = 13,
                   AG_Prod = 15
)

# rows of the inner model matrix
prod_path = rbind( 
        Patch =        c(0,0,0,0,0),
        urban.matrix = c(0,0,0,0,0),
        soil =         c(0,0,0,0,0),
        BG_Prod =      c(1,1,1,0,0),
        AG_Prod =      c(1,1,1,0,0)
)

# add column names (optional)
colnames(prod_path) = rownames(prod_path)
innerplot(prod_path)

# Check blocks
lapply(prod_blocks, function(x) names(data.prod)[x])

# Modes (A = reflective)
prod_modes = c("A","A","B", "A", "A")                              
# Run PLSPM analysis                      
prod_pls_env = plspm(data.prod, prod_path,
                     prod_blocks,
                     modes = prod_modes,
                     scaled = TRUE,
                     boot.val = TRUE,
                     br = 2000
)

prod_pls_env$unidim
prod_pls_env$inner_model
prod_pls_env$inner_summary
prod_pls_env$path_coefs
prod_pls_env$gof # 53%
prod_pls_env$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(prod_pls_env,
     arr.pos = 0.35,
     arr.lwd = abs(10*(prod_pls_env$path_coefs)) )
title("PLSPM productivity\nOnly Environmental factors", adj = 0)
mtext(3, text = paste("GOF=", round(prod_pls_env$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(2,
      text = paste(
              "R2(AG Prod)=",
              round(prod_pls_env$inner_summary["AG_Prod",2],2),
              "\nR2(BG Prod)=",
              round(prod_pls_env$inner_summary["BG_Prod",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 4)


quartz()
plot(prod_pls_env, what="loadings")
mtext(2,text = "PLSPM productivity\nOnly Environmental factors",
      las = 0, line= 1.3)


# PLSPM 3: Reduced variable types ####
# define Latent variables
prod_blocks = list(Patch = 1:2,
                   urban.matrix = 4:5,
                   Plant.div = 9,
                   BG_Prod = 13,
                   AG_Prod = 15
)

# rows of the inner model matrix
prod_path = rbind( 
        Patch =        c(0,0,0,0,0),
        urban.matrix = c(0,0,0,0,0),
        Plant.div =    c(1,1,0,0,0),
        BG_Prod =      c(1,1,1,0,0),
        AG_Prod =      c(1,1,1,0,0)
)

# add column names (optional)
colnames(prod_path) = rownames(prod_path)
innerplot(prod_path)

# Check blocks
lapply(prod_blocks, function(x) names(data.prod)[x])

# Modes (A = reflective)
prod_modes = c("A","A","A", "A", "A")                              
# Run PLSPM analysis                      
prod_pls_redux = plspm(data.prod, prod_path,
                     prod_blocks,
                     modes = prod_modes,
                     scaled = TRUE,
                     boot.val = TRUE,
                     br = 2000
)

prod_pls_redux$unidim
prod_pls_redux$inner_model
prod_pls_redux$inner_summary
prod_pls_redux$path_coefs
prod_pls_redux$gof # 57%
prod_pls_redux$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(prod_pls_redux,
     arr.pos = 0.35,
     arr.lwd = abs(10*(prod_pls_redux$path_coefs)) )
title("PLSPM productivity\nReduced variables", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(prod_pls_redux$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
              "R2(AG Prod)=",
              round(prod_pls_redux$inner_summary["AG_Prod",2],2),
              "\nR2(BG Prod)=",
              round(prod_pls_redux$inner_summary["BG_Prod",2],2),
              "\nR2(Plant Div)=",
              round(prod_pls_redux$inner_summary["Plant.div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)

quartz()
plot(prod_pls_redux, what="loadings")
mtext(2,text = "PLSPM productivity\nReduced variables", las = 0, line= 1.3)




# PLSPM 4: Reduced links & variables ####
# define Latent variables
prod_blocks = list(Patch = 1:2,
                   urban.matrix = 4:5,
                   Plant.div = 9,
                   BG_Prod = 13,
                   AG_Prod = 15
)

# rows of the inner model matrix
prod_path = rbind( 
  Patch =        c(0,0,0,0,0),
  urban.matrix = c(0,0,0,0,0),
  Plant.div =    c(1,0,0,0,0),
  BG_Prod =      c(0,1,1,0,0),
  AG_Prod =      c(1,0,1,0,0)
)

# add column names (optional)
colnames(prod_path) = rownames(prod_path)
innerplot(prod_path)

# Check blocks
lapply(prod_blocks, function(x) names(data.prod)[x])

# Modes (A = reflective)
prod_modes = c("A","A","A", "A", "A")                              
# Run PLSPM analysis                      
prod_pls_min = plspm(data.prod, prod_path,
                       prod_blocks,
                       modes = prod_modes,
                       scaled = TRUE,
                       boot.val = TRUE,
                       br = 2000
)

prod_pls_min$unidim
prod_pls_min$inner_model
prod_pls_min$inner_summary
prod_pls_min$path_coefs
prod_pls_min$gof # 57%
prod_pls_min$boot$paths

# Plot output of PLSPM
quartz()
plot(prod_pls_min, what="loadings")
mtext(2,text = "PLSPM productivity\nReduced variables", las = 0, line= 1.3)

quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(prod_pls_min,
     arr.pos = 0.35,
     arr.lwd = abs(10*(prod_pls_min$path_coefs)) )
title("PLSPM productivity\nReduced variables", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(prod_pls_min$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(AG Prod)=",
        round(prod_pls_min$inner_summary["AG_Prod",2],2),
        "\nR2(BG Prod)=",
        round(prod_pls_min$inner_summary["BG_Prod",2],2),
        "\nR2(Plant Div)=",
        round(prod_pls_min$inner_summary["Plant.div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)







## Plot model comparison ####

quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(env = prod_pls_env,
                             all = prod_pls_all,
                             redux = prod_pls_redux),
                multi = TRUE)

