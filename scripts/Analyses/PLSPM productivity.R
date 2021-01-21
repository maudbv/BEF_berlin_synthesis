# PLSPM to explain grassland AG and BG productivity (Biomass)

library(plspm)
nboot = 500
plot.graphs = FALSE

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
                           "AGB_C"
                           )]
)

data.prod = na.omit(data.prod)
dim(data.prod)
data.prod$TreeCover_patch <- 100 - data.prod$TreeCover_patch
names(data.prod)[names(data.prod)=="TreeCover_patch"] <- "Opp.TreeCover_patch"

# standardize:
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
data.prod <- as.data.frame(apply(data.prod,2,stdze))


# PLSPM 1: ALL selected variable types ####
plspm_prod_all <- run.plspm(
  data = data.prod,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                  Patch = c("log_Size_Patch","SVF"),
                  soil = c("N","P"),
                  Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
                  BG_Prod = c("BGB_C_total", "BGB_C_fine"),
                  AG_Prod = "AGB_C"),
  paths = "saturated endo",
  exo = 1:3,
  modes = c("A","B","B","A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 2: Only environmental ####
plspm_prod_env <- run.plspm(
  data = data.prod,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Patch = c("log_Size_Patch","SVF"),
                soil = c("N","P"),
                BG_Prod = c("BGB_C_total", "BGB_C_fine"),
                AG_Prod = "AGB_C"),
  paths = "saturated endo",
  exo = 1:3,
  modes = c("A","B","B", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)

# PLSPM 3: No urbanity ####
plspm_prod_nourb <- run.plspm(
  data = data.prod,
  blocks = list(Patch = c("log_Size_Patch","SVF"),
                soil = c("N","P"),
                Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
                BG_Prod = c("BGB_C_total", "BGB_C_fine"),
                AG_Prod = "AGB_C"),
  paths = "saturated endo",
  exo = 1:2,
  modes = c("B","B","A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)



# PLSPM 4: No mitigators/minimum ####
plspm_prod_min <- run.plspm(
  data = data.prod,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
                BG_Prod = c("BGB_C_total", "BGB_C_fine"),
                AG_Prod = "AGB_C"),
  paths = "saturated endo",
  exo = 1,
  modes = c("A","A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: Reduced paths ####
# define Latent variables
plspm_prod_redux <- run.plspm(
  data = data.prod,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Patch = c("log_Size_Patch","SVF"),
                soil = c("N","P"),
                Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
                BG_Prod = c("BGB_C_total", "BGB_C_fine"),
                AG_Prod = "AGB_C"),
  paths = rbind( 
    urban.matrix = c(0,0,0,0,0,0),
    Patch =        c(0,0,0,0,0,0),
    soil =         c(0,0,0,0,0,0),
    Plant.div =    c(1,1,1,0,0,0),
    BG_Prod =      c(1,0,1,1,0,0), # remove effect of patch on BG
    AG_Prod =      c(1,1,0,1,0,0)  # remove effect of soil and BG on AG
    ),
  exo = 1:3,
  modes = c("A","B","B","A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 6: Only diversity effect ####
# define Latent variables
plspm_prod_div <- run.plspm(
  data = data.prod,
  blocks = list(
                Plant.div = c("PlantHerb_SR","PlantHerb_Cover"),
                BG_Prod = c("BGB_C_total", "BGB_C_fine"),
                AG_Prod = "AGB_C"),
  paths ="saturated endo",
  exo = 1,
  modes = c("A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)



## Plot model comparison ####

quartz()
par(cex = 0.9)
plot.plspm.boot(model = list(environment_only = plspm_prod_env,
                             all = plspm_prod_all,
                             redux = plspm_prod_redux,
                             no_urbanisation = plspm_prod_nourb,
                             diversity_only = plspm_prod_div
                             ),
                multi = TRUE)

plot.plspm.boot(model =list(all = plspm_prod_all,
                            environment_only = plspm_prod_env,
                            diversity_only = plspm_prod_div
                            ),
multi = TRUE)
