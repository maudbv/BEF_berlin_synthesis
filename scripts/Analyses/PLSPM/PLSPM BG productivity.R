# PLSPM to explain grassland BG BGproductivity only

library(plspm)

## define parameters
nboot <- 1000 #bootstrap repetitions 
plot.graphs <- FALSE   # Should graphs be plotted for each analyses ?

# block of latent variables
blocks.BGprod = list(
  urban.matrix = c("Seal_500",
                   "mean_tempNight_summer"),
  Patch = c("Size_Patch","SVF"),
  soil = c("N","P"),
  Neophytes = "NeophyteHerb_Prop",
  Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
  BGprod = c("BGB_C_fine")
)

modes.BGprod= c("A","B","A","A","A", "A")

# PLSPM 1: ALL selected variable types ####
plspm_BGprod_all <- run.plspm(
  data = data.prod,
  blocks = blocks.BGprod,
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.BGprod,
  nboot = nboot,
  graph = plot.graphs)

quartz()
plot(plspm_BGprod_all, what = "loadings")

# PLSPM 2: Only environmental ####
plspm_BGprod_env <- run.plspm(
  data = data.prod,
  blocks = blocks.BGprod[-5],
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.BGprod[-5],
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 3: diversity effect only ####
plspm_BGprod_div <- run.plspm(
  data = data.prod,
  blocks = blocks.BGprod[c(5,6)],
  paths = "saturated endo",
  exo = 1,
  modes = modes.BGprod[c(5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 4: minimal model ####
plspm_BGprod_min <- run.plspm(
  data = data.prod,
  blocks = blocks.BGprod[c(1,5,6)],
  paths = "saturated endo",
  exo = 1,
  modes = modes.BGprod[c(1,5,6)],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: Reduced paths ####
# define Latent variables
plspm_BGprod_redux <- run.plspm(
  data = data.prod,
  blocks = blocks.BGprod[-c(2,4)], # remove neophytes
  paths = rbind(
    urban.matrix = c(0,0,0,0),
    soil =         c(0,0,0,0),
    Plant.div =    c(0,1,0,0),
    BGprod =       c(1,1,1,0)
  ),
  exo = 1:2,
  modes = modes.BGprod[-c(2,4)],
  nboot = nboot,
  graph = plot.graphs)


## Plot model comparison ####
quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(Envir = plspm_BGprod_env,
                             Full_model = plspm_BGprod_all,
                             Reduced = plspm_BGprod_redux,
                             Urban_BEF = plspm_BGprod_min,
                             BEF = plspm_BGprod_div
),
multi = TRUE, plot.legend = TRUE)

