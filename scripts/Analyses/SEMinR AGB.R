# PLS-PM to explain grassland aboveground productivity (AG Plant Biomass)
# using new package "seminr"
# https://cran.r-project.org/web/packages/seminr/vignettes/SEMinR.html#introduction

library(lavaan)
library(seminr)

# example from package  ####
measurements <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = "Expectation")
)

  # Quickly create multiple paths "from" and "to" sets of constructs  
  structure <- relationships(
    paths(from = c("Image", "Expectation", "Image*Expectation"), to = "Value"),
    paths(from = "Value", to = "Satisfaction")
  )


  # Estimate using PLS-PM from model parts defined earlier  
  pls_model <- estimate_pls(data = mobi, 
                            measurement_model = measurements, 
                            structural_model = structure)
summary(pls_model)

# note: PLS requires seperate bootstrapping for PLS path estimates
# SEMinR uses multi-core parallel processing to speed up bootstrapping
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
summary(boot_estimates)

# Alternatively, we could estimate our model using CBSEM, which uses the Lavaan package
# We often wish to conduct a CFA of our measurement model prior to CBSEM
# note: we must convert composites in our measurement model into reflective constructs for CFA/CBSEM
cfa_model <- estimate_cfa(data = mobi, as.reflective(measurements))
summary(cfa_model)

cbsem_model <- estimate_cbsem(data = mobi, as.reflective(measurements), structure)
summary(cbsem_model)

# note: the Lavaan syntax and Lavaan fitted model can be extracted for your own specific needs
cbsem_model$lavaan_syntax
cbsem_model$lavaan_model

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
                                 "NeophyteHerb_Prop"
                                )],
                   EF_data[, c(
                           "AGB_C","BGB_C_total"
                             )]
)

data.prod = na.omit(data.prod)
dim(data.prod)
data.prod$TreeCover_patch <- 100 - data.prod$TreeCover_patch
names(data.prod)[names(data.prod)=="TreeCover_patch"] <- "Opp.TreeCover_patch"

# standardize: 
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
data.prod <- as.data.frame(apply(data.prod,2,stdze))

# check: 
varTable(data.prod)

# define Latent variables with "composite" constructs (= formative) ####
measurements <- constructs(
  composite("patch_size", colnames(data.prod)[1]),
  composite("urban_matrix",  colnames(data.prod)[4:5]),
  composite("soil_N",        colnames(data.prod)[6]),
  composite("plant_div", colnames(data.prod)[9:10]),
  composite("AG_biomass", colnames(data.prod)[13]),
  interaction_term(iv = "plant_div", moderator = "urban_matrix")
)

# MODEL 1: ALL with interactions ####
structure <- relationships(
  paths(from = c("patch_size", "soil_N","urban_matrix"),
        to = "plant_div"),
  paths(from = c("patch_size","urban_matrix","plant_div","soil_N", 
                 "plant_div*urban_matrix"),
        to = "AG_biomass")
)
# Run PLS-SEM model (                
pls_model_all <- estimate_pls(data = data.prod, 
                               measurement_model = measurements, 
                               structural_model = structure)
summary(pls_model_all) #43%

boot_estimates <- bootstrap_model(pls_model_all, nboot = 1000, cores = 2)

# Return conf intervals: 
confidence_interval(boot_seminr_model = boot_estimates,
                    from = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)

confidence_interval(boot_seminr_model = boot_estimates,
                    from = "urban_matrix",
                    through = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)

confidence_interval(boot_seminr_model = boot_estimates,
                    from = "patch_size",
                    through = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)

confidence_interval(boot_seminr_model = boot_estimates,
                    from = "plant_div*urban_matrix",
                    to = "AG_biomass",
                    alpha = 0.05)
# NOT GREAT!

## NO URBANITY MODEL #####
structure <- relationships(
  paths(from = c("soil_N", "patch_size"),
        to = "plant_div"),
  paths(from = c("soil_N", "plant_div"),
        to = "AG_biomass")
)
pls_model <- estimate_pls(data = data.prod, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)

# bootstrap
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)

# Return conf intervals: 
confidence_interval(boot_seminr_model = boot_estimates,
                    from = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)

## no diversity MODEL #####
structure <- relationships(
  paths(from = c("urban_matrix", "soil_N", "patch_size"),
        to = "AG_biomass")
)
pls_model <- estimate_pls(data = data.prod, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model) # 29 %

# bootstrap
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)

# Return conf intervals: 
confidence_interval(boot_seminr_model = boot_estimates,
                    from = "urban_matrix",
                    to = "AG_biomass",
                    alpha = 0.05)


## no interaction MODEL #####
structure <- relationships(
  paths(from = c("patch_size", "soil_N","urban_matrix"),
        to = "plant_div"),
  paths(from = c("patch_size","urban_matrix","plant_div","soil_N"),
        to = "AG_biomass")
)

pls_model <- estimate_pls(data = data.prod, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model) # 36 % (interaction was 43%)

# bootstrap
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)

# Return conf intervals: 
confidence_interval(boot_seminr_model = boot_estimates,
                    from = "urban_matrix",
                    to = "AG_biomass",
                    alpha = 0.05)
confidence_interval(boot_seminr_model = boot_estimates,
                    from = "patch_size",
                    to = "AG_biomass",
                    alpha = 0.05)



## no interaction MODEL #####
structure <- relationships(
  paths(from = c("patch_size", "soil_N","urban_matrix"),
        to = "plant_div"),
  paths(from = c("patch_size","plant_div",
                 "plant_div*urban_matrix"),
        to = "AG_biomass")
)

pls_model <- estimate_pls(data = data.prod, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model) # 41 %  (30% adj) (full was 43%, 22% adj) => better than full

# bootstrap
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)

# Return conf intervals: 
confidence_interval(boot_seminr_model = boot_estimates,
                    from = "urban_matrix",
                    through = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)

confidence_interval(boot_seminr_model = boot_estimates,
                    from = "patch_size",
                    through = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)

confidence_interval(boot_seminr_model = boot_estimates,
                    from = "patch_size",
                    to = "AG_biomass",
                    alpha = 0.05)

confidence_interval(boot_seminr_model = boot_estimates,
                    from = "plant_div",
                    to = "AG_biomass",
                    alpha = 0.05)
