# PLSPM to explain grassland decompuctivity (Biomass)

library(plspm)

## Select factors
data.decomp <- cbind(Env_data[EF_data$ID_plot,
                            c("pH",
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


# define list of indicators: what variables are associated with # what latent variables
decomp_blocks = list(Soil = 1:4, 
                     urban.matrix = 5:6,
                     History = 7,
                     Vegetation = 8:10,
                     Neophytes = 11:12,
                     Decomposers.Div = 13:16,
                     Decomp.s = 17,
                     Decomp.k = 18) 

# Check simple pearson correlations
corMat <- cor(data.decomp[,unlist(decomp_blocks)],
              use = "pairwise.complete.obs",
              method = "pearson") 

## Ring layout for correlations
quartz()
qgraph(corMat, graph = "cor",
                         layout = "circle",
                         threshold = FALSE, alpha = 0.05,
                         sampleSize = nrow(data.decomp), 
                         groups = substr(names(unlist(decomp_blocks)),1,4),
                         nodeNames = colnames(corMat),
                         legend.cex = 0.4,
                         cut = 0, minimum = "sig", maximum = 1,
                         asize = 10,
                         details = TRUE,
                         palette = "pastel",
                         theme = "Borkulo",
                         title = "decomposition correlations")


# PLSPM 1: ALL selected variable types ####

# define latent variables
decomp_blocks = list(Soil = 1:4, 
                     urban.matrix = 5:6,
                     History = 7,
                     Vegetation = 8:10,
                     Neophytes = 11:12,
                     Decomposers.Div = 13:16,
                     Decomp = 17:18) 

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])


# rows of the inner model matrix
decomp_path = rbind(Soil =         c(0,0,0,0,0,0,0),
                    urban.matrix = c(0,0,0,0,0,0,0),
                    History =      c(0,0,0,0,0,0,0),
                    Vegetation =   c(0,0,0,0,0,0,0),
                    Neophytes =    c(0,0,0,0,0,0,0),
                    Decomposers.Div =    c(1,1,1,1,1,0,0),
                    Decomp =       c(1,1,1,1,1,1,0)
)

# add column names (optional)
colnames(decomp_path) = rownames(decomp_path)
innerplot(decomp_path)

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])

# Modes (A = reflective)
decomp_modes <- rep("A", length(decomp_blocks))
decomp_modes[c(1:2,6)] <- "B"
decomp_modes[length(decomp_blocks)] <- "B"

# Run PLSPM analysis                 
decomp_pls_all <- plspm(data.decomp, decomp_path,
                        decomp_blocks,
                        modes = decomp_modes,
                        scaled = TRUE,
                        boot.val = TRUE,
                        br = 2000
)

decomp_pls_all$unidim
decomp_pls_all$inner_model
decomp_pls_all$inner_summary
decomp_pls_all$path_coefs
decomp_pls_all$gof # 56
decomp_pls_all$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(decomp_pls_all,
     arr.pos = 0.35,
     arr.lwd = abs(10*(decomp_pls_all$path_coefs)) )
title("PLSPM Decomposition\nall groups", adj = 0)
mtext(3, text = paste("GOF=", round(decomp_pls_all$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(Decomposition)=",
        round(decomp_pls_all$inner_summary["Decomp",2],2),
        "\nR2(Decomp. Div)=",
        round(decomp_pls_all$inner_summary["Decomposers.Div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0)

plot.plspm.boot(decomp_pls_all)

quartz()
plot(decomp_pls_all, what="loadings")
mtext(2,text = "PLSPM Decomposition\nall groups", 
      las = 0, line= 1.3)



## PLSPM for DECOMPOSITION RATE (K) ####

# PLSPM 1: ALL selected variable types ####

# define latent variables
decomp_blocks = list(Soil = 1:4, 
                     urban.matrix = 5:6,
                     History = 7,
                     Vegetation = 8:10,
                     Neophytes = 11:12,
                     Decomposers.Div = 13:16,
                     Decomp = 18) 

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])


# rows of the inner model matrix
decomp_path = rbind(Soil =         c(0,0,0,0,0,0,0),
                    urban.matrix = c(0,0,0,0,0,0,0),
                    History =      c(0,0,0,0,0,0,0),
                    Vegetation =   c(0,0,0,0,0,0,0),
                    Neophytes =    c(0,0,0,0,0,0,0),
                    Decomposers.Div =    c(1,1,1,1,1,0,0),
                    Decomp =       c(1,1,1,1,1,1,0)
)

# add column names (optional)
colnames(decomp_path) = rownames(decomp_path)
innerplot(decomp_path)

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])

# Modes (A = reflective)
decomp_modes <- rep("A", length(decomp_blocks))
decomp_modes[c(1:2,6)] <- "B"
decomp_modes[length(decomp_blocks)] <- "A"

# Run PLSPM analysis                 
decomp_pls_all <- plspm(data.decomp, decomp_path,
                        decomp_blocks,
                        modes = decomp_modes,
                        scaled = TRUE,
                        boot.val = TRUE,
                        br = 2000
)

decomp_pls_all$unidim
decomp_pls_all$inner_model
decomp_pls_all$inner_summary
decomp_pls_all$path_coefs
decomp_pls_all$gof # 56
decomp_pls_all$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(decomp_pls_all,
     arr.pos = 0.35,
     arr.lwd = abs(10*(decomp_pls_all$path_coefs)) )
title("PLSPM Decomposition\nall groups", adj = 0)
mtext(3, text = paste("GOF=", round(decomp_pls_all$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(Decomposition)=",
        round(decomp_pls_all$inner_summary["Decomp",2],2),
        "\nR2(Decomp. Div)=",
        round(decomp_pls_all$inner_summary["Decomposers.Div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0)

plot.plspm.boot(decomp_pls_all)

quartz()
plot(decomp_pls_all, what="loadings")
mtext(2,text = "PLSPM Decomposition\nall groups", 
      las = 0, line= 1.3)




# PLSPM 2: Only environmental ####
# define latent variables
decomp_blocks = list(Soil = 1:4, 
                     urban.matrix = 5:6,
                     History = 7,
                     Vegetation = 8:10,
                     Neophytes = 11:12,
                     Decomp = 18) 

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])


# rows of the inner model matrix
decomp_path = rbind(Soil =         c(0,0,0,0,0,0),
                    urban.matrix = c(0,0,0,0,0,0),
                    History =      c(0,0,0,0,0,0),
                    Vegetation =   c(0,0,0,0,0,0),
                    Neophytes =    c(0,0,0,0,0,0),
                    Decomp =       c(1,1,1,1,1,0)
)
# add column names (optional)
colnames(decomp_path) = rownames(decomp_path)
innerplot(decomp_path)

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])

# Modes (A = reflective)
decomp_modes <- rep("A", length(decomp_blocks))
decomp_modes[c(1)] <- "B"
decomp_modes[length(decomp_blocks)] <- "A"

# Run PLSPM analysis                      
decomp_pls_env = plspm(data.decomp, decomp_path,
                     decomp_blocks,
                     modes = decomp_modes,
                     scaled = TRUE,
                     boot.val = TRUE,
                     br = 2000
)

decomp_pls_env$unidim
decomp_pls_env$inner_model
decomp_pls_env$inner_summary
decomp_pls_env$path_coefs
decomp_pls_env$gof # 53%
decomp_pls_env$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(decomp_pls_env,
     arr.pos = 0.35,
     arr.lwd = abs(10*(decomp_pls_env$path_coefs)) )
title("PLSPM Decomposition\nOnly Environmental factors", adj = 0)
mtext(3, text = paste("GOF=", round(decomp_pls_env$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(2,
      text = paste(
        "R2(Decomposition)=",
        round(decomp_pls_env$inner_summary["Decomp",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 4)

plot.plspm.boot(decomp_pls_env)

quartz()
plot(decomp_pls_env, what="loadings")
mtext(2,text = "PLSPM Decomposition\nOnly Environmental factors", las = 0, line= 1.3)


# PLSPM 3: Reduced variable types ####
# define latent variables
decomp_blocks = list(Soil = 1:4, 
                     urban.matrix = c(5:6,11),
                     Vegetation = 8:10,
                     Decomposers.Div = 13:16,
                     Decomp = 18
) 


# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])


# rows of the inner model matrix
decomp_path = rbind(Soil=          c(0,0,0,0,0),
                    urban.matrix = c(0,0,0,0,0),
                    Vegetation =   c(0,0,0,0,0),
                    Decomposers.Div =  c(1,1,1,0,0),
                    Decomp =       c(1,1,1,1,0)
) 
# add column names (optional)
colnames(decomp_path) = rownames(decomp_path)
innerplot(decomp_path)

# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])

# Modes (A = reflective)
decomp_modes = c("B","A","B", "B","A")                              
# Run PLSPM analysis                      
decomp_pls_redux = plspm(data.decomp, decomp_path,
                       decomp_blocks,
                       modes = decomp_modes,
                       scaled = TRUE,
                       boot.val = TRUE,
                       br = 2000
)

decomp_pls_redux$unidim
decomp_pls_redux$inner_model
decomp_pls_redux$inner_summary
decomp_pls_redux$path_coefs
decomp_pls_redux$gof # 57%
decomp_pls_redux$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(decomp_pls_redux,
     arr.pos = 0.35,
     arr.lwd = abs(10*(decomp_pls_redux$path_coefs)) )
title("PLSPM Decomposition\nReduced variables", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(decomp_pls_redux$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(decomp)=",
        round(decomp_pls_redux$inner_summary["Decomp",2],2),
        "\nR2(decomp Div)=",
        round(decomp_pls_redux$inner_summary["Decomposers.Div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)

plot.plspm.boot(decomp_pls_redux)

quartz()
plot(decomp_pls_redux, what="loadings")
mtext(2,text = "PLSPM Decomposition\nReduced variables", las = 0, line= 1.3)


quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(env = decomp_pls_env,
                             all = decomp_pls_all,
                             redux = decomp_pls_redux),
                multi = TRUE)






## PLSPM for DECOMPOSITION MAX (S) ####
# data.decomp$NeophyteHerb_Prop <-  1 - data.decomp$NeophyteHerb_Prop 
# data.decomp$Neophyte_RelCover <-  1 - data.decomp$Neophyte_RelCover
data.decomp$pH <-   - data.decomp$pH

decomp_blocks = list(urbanisation = c(5:6,11),
                     Age = 7,
                     Vegetation = 8:10,
                     Soil = 1:4, 
                     Decomposers.Div = 13:16,
                     Decomp.s = 17
) 


# Check blocks
lapply(decomp_blocks, function(x) names(data.decomp)[x])


# rows of the inner model matrix
decomp_path = rbind(urbanisation = c(0,0,0,0,0,0),
                    Age =          c(0,0,0,0,0,0),
                    Vegetation =   c(0,0,0,0,0,0),
                    Soil   =       c(1,1,1,0,0,0),
                    Decomposers.Div =  c(1,1,1,1,0,0),
                    Decomp.s =     c(1,1,1,1,1,0)
) 

# add column names (optional)
colnames(decomp_path) = rownames(decomp_path)
innerplot(decomp_path)


# Modes (A = reflective)
decomp_modes = c("A","A","A","B","B","A")               

# Run PLSPM analysis                      
decomp_pls = plspm(data.decomp, decomp_path,
                   decomp_blocks,
                   modes = decomp_modes,
                   scaled = TRUE,
                   boot.val = TRUE,
                   br = 2000
)

# Plot output of PLSPM

plot(decomp_pls,
     arr.pos = 0.35,
     arr.lwd = abs(10*(decomp_pls$path_coefs)) )

plot.plspm.boot(decomp_pls)

plot(decomp_pls, what="loadings")

decomp_pls$unidim
decomp_pls$inner_model
decomp_pls$inner_summary
decomp_pls$path_coefs
decomp_pls$gof # 51%
decomp_pls$boot$paths


