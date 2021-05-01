# Trying out PLSPM

library(plspm)
# define parameters:
nboot <- 1000 #bootstrap repetitions 
plot.graphs <- TRUE # draw graphs for each run ?

# define blocks of latent variables:
blocks.poll = list(urban.matrix = c("Seal_500",
                                    "mean_tempNight_summer"),
              Patch = c("Size_Patch","SVF"),
              connectivity = c("Hanski3D_DryGr","ShDry_500"),
              Vegetation= c("Plants_insect.poll_Cover",
                            "Plants_insect.poll_SR"),
              Poll.div  = c("Wildbees_polylectic_SR",
                            "Pollinators_SR",
                            "Pollinators_Abun"),
              Pollination = c("Poll.visits")
)

modes.poll =  c("A","A","A","A", "A", "A")

# PLSPM 1: ALL selected variable types ####
plspm_poll_all <- run.plspm(
  data = data.poll,
  blocks = blocks.poll,
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.poll,
  nboot = nboot,
  graph = TRUE)

quartz()
plot(plspm_poll_all, what = "loadings")

# PLSPM 1bis: ALL selected variable types - no patch ####
plspm_poll_all_nopatch <- run.plspm(
  data = data.poll,
  blocks = blocks.poll[-2], # remove element 2 = "Patch"
  paths = "saturated endo",
  exo = 1:3,
  modes = modes.poll[-2],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 2: Only environmental direct ####
plspm_poll_env <- run.plspm(
  data = data.poll,
  blocks = blocks.poll[c("urban.matrix","Patch",
                         "connectivity","Vegetation",
                         "Pollination")],
  paths = "saturated endo",
  exo = 1:4,
  modes = modes.poll[-5],
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 3: diversity effect only ####
plspm_poll_div <- run.plspm(
  data = data.poll,
  blocks = blocks.poll[c("Poll.div","Pollination")],
  paths = "saturated endo",
  exo = 1,
  modes = c("A", "A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 4: minimal model ####

plspm_poll_min <- run.plspm(
  data = data.poll,
  blocks = blocks.poll[c("urban.matrix","Poll.div","Pollination")],
  paths = "saturated endo",
  exo = 1,
  modes = modes.poll,
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: Reduced variable links ####
plspm_poll_redux <- run.plspm(
  data = data.poll,
  blocks = blocks.poll[-2], #no patch LV
  paths = rbind( 
    urban.matrix = c(0,0,0,0,0),
    connectivity = c(0,0,0,0,0),
    Vegetation =   c(0,0,0,0,0), 
    Poll.div =     c(1,0,0,0,0), # remove effect of veg on poll div
    Pollination =  c(0,1,1,1,0)  # remove effect of urban & Patch on poll
  ),
  exo = 1:2,
  modes = c("A","A","A","A", "A"),
  nboot = nboot,
  graph = TRUE)

## Plot model comparison ####
quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(Envir = plspm_poll_env,
                             Full_model = plspm_poll_all,
                             Reduced = plspm_poll_redux,
                             Urban_BEF = plspm_poll_min,
                             BEF = plspm_poll_div
),
multi = TRUE, plot.legend = TRUE)

