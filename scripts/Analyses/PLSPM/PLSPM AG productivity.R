# PLSPM to explain grassland AG AGproductivity only

library(plspm)

## define parameters
nboot <- 1000 #bootstrap repetitions 
plot.graphs <-  FALSE # draw graphs for each run ?

# block of latent variables
blocks.AGprod = list(
  urban.matrix = c("Seal_500",
                   "mean_tempNight_summer"),
  Patch = c("Size_Patch","SVF"),
  soil = c("N","P"),
  Neophytes = "NeophyteHerb_Prop",
  Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
  AGprod = c("AGB_C")
)

modes.AGprod= c("A","A","A","A","A", "A")

# PLSPM 1: ALL selected variable types ####
plspm_AGprod_all <- run.plspm(
  data = data.prod,
  blocks = blocks.AGprod,
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.AGprod,
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_AGprod_all, what = "loadings")

# PLSPM 2: Only environmental ####
plspm_AGprod_env <- run.plspm(
  data = data.prod,
  blocks = blocks.AGprod[-5],
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.AGprod[-5],
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 3: diversity effect only ####
plspm_AGprod_div <- run.plspm(
  data = data.prod,
  blocks = blocks.AGprod[c(5,6)],
  paths = "saturated endo",
  exo = 1,
  modes = modes.AGprod[c(5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 4: minimal model ####
plspm_AGprod_min <- run.plspm(
  data = data.prod,
  blocks = blocks.AGprod[c(1,5,6)],
  paths = "saturated endo",
  exo = 1,
  modes = modes.AGprod[c(1,5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: Reduced paths ####
plspm_AGprod_redux <- run.plspm(
  data = data.prod,
  blocks = blocks.AGprod[-c(1,4)], # remove urban + neophytes
  paths = rbind(
    Patch =        c(0,0,0,0),
    soil =         c(0,0,0,0),
    Plant.div =    c(1,1,0,0),
    AGprod =       c(1,0,1,0)
  ),
  exo = 1:2,
  modes = modes.AGprod[-c(1,4)],
  nboot = nboot,
  graph = plot.graphs)

## Plot model comparison ####
quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(Envir = plspm_AGprod_env,
                             Full_model = plspm_AGprod_all,
                             Reduced = plspm_AGprod_redux,
                             Urban_BEF = plspm_AGprod_min,
                             BEF = plspm_AGprod_div
),
multi = TRUE, plot.legend = TRUE)

