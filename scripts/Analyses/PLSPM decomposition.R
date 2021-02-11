# PLSPM to explain grassland decompuctivity (Biomass)

library(plspm)

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

# Log transform richness and abundance
var2log <-c("BG_Decomposer_Abun","Decomposer_Abun",
            "BG_Decomposer_TR","Decomposer_SR",
            "HanskiHist",
            "P","N","KAK","C")

data.decomp[, var2log] <- log(data.decomp[, var2log])

# variables to sqrt transform (contain zero - also good normal fit)
var2sqrt <-c("NeophyteHerb_Prop",
             "Neophyte_RelCover") 
# sqrt transformation does not work so great for RelCover
data.decomp[, var2sqrt] <- sqrt(data.decomp[, var2sqrt])

## OPTION standardize variables :
# stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
# data.decomp <- as.data.frame(apply(data.decomp,2,stdze))

# Check distributions: 
quartz()
par (mar = c(4,1,1,1))
hist(data.decomp, nclass = 6)


## define parameters ####
nboot <- 1000 #bootstrap repetitions 
plot.graphs <- TRUE # draw graphs for each run ?

# PLSPM 1: ALL selected variable types ####
plspm_decomp_all <- run.plspm(
  data = data.decomp,
  blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
                Soil = c("P","N","KAK"),
                History = c("HanskiHist"),
                Vegetation = c("Cover_total","Cover_litter", "Plant_SR"),
                Neophytes = c("NeophyteHerb_Prop","Neophyte_RelCover"),
                Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
                                "Decomposer_Abun","BG_Decomposer_Abun"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = "saturated endo",
  exo = 1:5,
  modes = c("A","B","A","B","A", "B", "A"),
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_decomp_all, what = "loadings")

# PLSPM 2: environmental, no diversity effect  ####
plspm_decomp_env <- run.plspm(
  data = data.decomp,
  blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
                Soil = c("P","N","KAK"),
                History = c("HanskiHist"),
                Vegetation = c("Cover_total","Cover_litter", "Plant_SR"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = "saturated endo",
  exo = 1:4,
  modes = c("A","B","A","B", "A"),
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_decomp_env, what = "loadings")


# PLSPM 3: Diversity only ####
plspm_decomp_div <- run.plspm(
  data = data.decomp,
  blocks = list(Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
                                "Decomposer_Abun","BG_Decomposer_Abun"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = "saturated endo",
  exo = 1,
  modes = c("B","A"),
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_decomp_div, what = "loadings")


# PLSPM 4: Minimum paths ####
plspm_decomp_min <- run.plspm(
  data = data.decomp,
  blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
                Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
                                "Decomposer_Abun","BG_Decomposer_Abun"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = "saturated endo",
  exo = 1,
  modes = c("A", "B", "A"),
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_decomp_min, what = "loadings")

plot(plspm_decomp_min)

# PLSPM 5: Reduced  ####
plspm_decomp_redux <- run.plspm(
  data = data.decomp,
  blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
                Soil = c("P","N","KAK"),
                Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
                                "Decomposer_Abun","BG_Decomposer_Abun"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = rbind(
    urban.matrix =        c(0,0,0,0),
    Soil =                c(0,0,0,0),  
    # negative Urban effect on soil is interesting but not in the initial hyp
    Decomp_Div =          c(1,1,0,0),
    Decomposition.rate =  c(1,0,1,0)
  ),
  exo = 1:2,
  modes = c("A","B","B","A"),
  nboot = nboot,
  graph = plot.graphs)
# 
# 
# plspm_decomp_redux <- run.plspm(
#   data = data.decomp,
#   blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
#                 Soil = c("P","N","KAK"),
#                 Vegetation = c("Cover_total","Cover_litter", "Plant_SR"),
#                 Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
#                                 "Decomposer_Abun","BG_Decomposer_Abun"),
#                 Decomposition.rate = c("decomposition_k")
#   ),
#   paths = rbind(
#     urban.matrix =        c(0,0,0,0,0),
#     Soil =                c(0,0,0,0,0),
#     Vegetation =          c(0,0,0,0,0),
#     Decomp_Div =          c(1,1,1,0,0),
#     Decomposition.rate =  c(1,0,1,1,0)
#     ),
#   exo = 1:4,
#   modes = c("A","B","B", "B", "A"),
#   nboot = nboot,
#   graph = plot.graphs)

quartz()
plot(plspm_decomp_redux, what = "loadings")


## Plot model comparison ####

quartz()
par(mfrow= c(1,1), cex = 0.9)
plot.plspm.boot(model = list(environment_only = plspm_decomp_env,
                             all = plspm_decomp_all,
                             redux = plspm_decomp_redux,
                             min = plspm_decomp_min,
                             diversity_only = plspm_decomp_div
),
multi = TRUE)

