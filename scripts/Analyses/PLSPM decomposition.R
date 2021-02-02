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
data.decomp[, c("BG_Decomposer_Abun","Decomposer_Abun",
                "BG_Decomposer_TR","Decomposer_SR")] <- log(
                  data.decomp[, c("BG_Decomposer_Abun","Decomposer_Abun",
                                  "BG_Decomposer_TR","Decomposer_SR")])
# # standardize:
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
data.decomp <- as.data.frame(apply(data.decomp,2,stdze))



# Store data table:
write.csv(data.decomp, 
          file = "decomposition data.csv")

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
                Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
                                "Decomposer_Abun","BG_Decomposer_Abun"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = "saturated endo",
  exo = 1:4,
  modes = c("A","B","A","B", "B", "A"),
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


# PLSPM 4: Mininimum paths ####
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

# PLSPM 5: Reduced  ####
plspm_decomp_redux <- run.plspm(
  data = data.decomp,
  blocks = list(urban.matrix = c("Seal_500","mean_temp_summer"),
                Soil = c("P","N","KAK"),
                Vegetation = c("Cover_total","Cover_litter", "Plant_SR"),
                Decomp_Div  = c("BG_Decomposer_TR","Decomposer_SR",
                                "Decomposer_Abun","BG_Decomposer_Abun"),
                Decomposition.rate = c("decomposition_k")
  ),
  paths = "saturated endo",
  exo = 1:3,
  modes = c("A","B","B", "B", "A"),
  nboot = nboot,
  graph = plot.graphs)

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
