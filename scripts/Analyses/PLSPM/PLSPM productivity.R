# PLSPM to explain grassland AG and BG productivity together (Biomass)

library(plspm)

## define parameters
nboot <- 1000 #bootstrap repetitions 
plot.graphs <- FALSE # draw graphs for each run ?

# block of latent variables
blocks.prod = list(
  urban.matrix = c("Seal_500",
                   "mean_tempNight_summer"),
  Patch = c("Size_Patch","SVF"),
  soil = c("N","P"),
  Neophytes = "NeophyteHerb_Prop",
  Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
  Prod = c("BGB_C_total","BGB_C_fine", "AGB_C")
)

# modes.prod= c("A","B","B","A","A", "B")
modes.prod= c("A","B","A","A","A", "B")

# PLSPM 1: ALL selected variable types ####
plspm_prod_all <- run.plspm(
  data = data.prod,
  blocks = blocks.prod,
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.prod,
  nboot = nboot,
  graph =TRUE)

quartz()
plot(plspm_prod_all, what = "loadings")

# PLSPM 2: Only environmental ####
plspm_prod_env <- run.plspm(
  data = data.prod,
  blocks = blocks.prod[-5],
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.prod[-5],
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 3: diversity effect only ####
plspm_prod_div <- run.plspm(
  data = data.prod,
  blocks = blocks.prod[c(5,6)],
  paths = "saturated endo",
  exo = 1,
  modes = modes.prod[c(5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 4: minimal model ####
plspm_prod_min <- run.plspm(
  data = data.prod,
  blocks = blocks.prod[c(1,5,6)],
  paths = "saturated endo",
  exo = 1,
  modes = modes.prod[c(1,5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: Reduced paths ####
# define Latent variables
plspm_prod_redux <- run.plspm(
  data = data.prod,
  blocks = blocks.prod[-4], # remove neophytes
  paths = rbind(  
    urban.matrix = c(0,0,0,0,0),
    Patch =        c(0,0,0,0,0),
    soil =         c(0,0,0,0,0),
    Plant.div =    c(0,0,1,0,0),
    Prod =         c(1,1,0,1,0)
    ),
  exo = 1:3,
  modes = modes.prod[-4],
  nboot = nboot,
  graph = TRUE)


## Plot model comparison ####
quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(Envir = plspm_prod_env,
                             Full_model = plspm_prod_all,
                             Reduced = plspm_prod_redux,
                             Urban_BEF = plspm_prod_min,
                             BEF = plspm_prod_div
),
multi = TRUE, plot.legend = TRUE)

