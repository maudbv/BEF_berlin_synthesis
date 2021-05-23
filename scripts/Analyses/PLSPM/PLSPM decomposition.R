# PLSPM to explain grassland decompuctivity (Biomass)

library(plspm)

## define parameters
nboot <- 1000 #bootstrap repetitions 
plot.graphs <- FALSE # draw graphs for each run ?

# block of latent variables
blocks.decomp = list(
  urban.matrix =     c("Seal_500","mean_tempNight_summer"), 
  Soil =             c("P","N","KAK"),
  History =          c("HanskiHist"),
  Vegetation =       c("Cover_total","Cover_litter", "Plant_SR"),
  Decomp_Div  =      c("BG_Decomposer_TR","Decomposer_SR",
                        "Decomposer_Abun","BG_Decomposer_Abun"),
  Decomposition.rate =    c("decomposition_k")
)

modes.decomp = c("A","B","A","B", "B", "A")

# PLSPM 1: ALL selected variable types ####
plspm_decomp_all <- run.plspm(
  data = data.decomp,
  blocks = blocks.decomp,
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.decomp,
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_decomp_all, what = "loadings")

# PLSPM 2: environmental, no diversity effect  ####
plspm_decomp_env <- run.plspm(
  data = data.decomp,
  blocks = blocks.decomp[c("urban.matrix" ,"Soil","History",
                           "Vegetation","Decomposition.rate")],
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.decomp[-5],
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 2is : environmental, with diversity effect  ####
# plspm_decomp_env_div <- run.plspm(
#   data = data.decomp,
#   blocks = blocks.decomp,
#   paths = "saturated endo",
#   exo = 1:5,
#   modes = modes.decomp,
#   nboot = nboot,
#   graph = plot.graphs)
# 
# PLSPM 3: Diversity only ####
plspm_decomp_div <- run.plspm(
  data = data.decomp,
  blocks = blocks.decomp[c("Decomp_Div","Decomposition.rate")],
  paths = "saturated endo",
  exo = 1,
  modes = modes.decomp[5:6],
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 4: Minimum paths ####
plspm_decomp_min <- run.plspm(
  data = data.decomp,
  blocks = blocks.decomp[c("urban.matrix",
                           "Decomp_Div","Decomposition.rate")],
  paths = "saturated endo",
  exo = 1,
  modes = modes.decomp[c(1,5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: Reduced  ####
plspm_decomp_redux <- run.plspm(
  data = data.decomp,
  blocks = blocks.decomp[c("urban.matrix", "Soil",
                           "Decomp_Div","Decomposition.rate")],
  paths = rbind(
    urban.matrix =        c(0,0,0,0),
    Soil =                c(0,0,0,0),  
    # negative Urban effect on soil is interesting but not in the initial hyp
    Decomp_Div =          c(1,1,0,0),
    Decomposition.rate =  c(1,0,1,0)
  ),
  exo = 1:2,
  modes = modes.decomp[c(1,2,5,6)],
  nboot = nboot,
  graph = plot.graphs)


## Plot model comparison ####

quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(Envir = plspm_decomp_env,
                             Full_model = plspm_decomp_all,
                             Reduced = plspm_decomp_redux,
                             Urban_BEF = plspm_decomp_min,
                             BEF = plspm_decomp_div
),
multi = TRUE, plot.legend = TRUE)

