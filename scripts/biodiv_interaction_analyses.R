
# necessary functions
source('scripts/functions/FUNCTION add stats.R')
source('scripts/functions/p2star.R')
source('scripts/functions/glm.biodiv.R')

# Selection of plots
plots_selected_20 <- Env_data$ID_plot[which(Env_data$Subset_20plots == 1) ]
plots_all<- Env_data$ID_plot

# Biodiversity data
Biodiv_data_modif <- Biodiv_data

# remove outliers
Biodiv_data_modif["Nl_14", "Legume.cover"]  <- NA
# Biodiv_data_modif["Nl_14", "Neophyte.cover"]  <- NA
Biodiv_data_modif["Ol_55", "BG_Predator_Abun"]  <- NA
# Biodiv_data_modif["Nl_14", "BG_Predator_Abun"]  <- NA
Biodiv_data_modif["Nm_09", "Decomposer_Abun"  ]  <- NA

# Modify Env_data 
Env_data_modif <- Env_data

# outliers
Env_data_modif ["Nm_05", "ShHerb_500"]   <- NA
Env_data_modif ["Nm_09", "N"]   <- NA


# Select variables
richness_indices <- c(  "Plant_SR", "Grass_SR", "Legume_SR",
                        "Forb_SR", "Moss_SR" , "Phanerophyte_SR",
                        "Indigenous_SR","Archeophyte_SR","Neophyte_SR","Neophyte.prop",
                        "NeophyteHerb_SR","NeophyteHerb.prop",
                        "Decomposer_SR", "Herbivores_SR",
                        "Predators_SR", "Pollinators_SR",
                        "BG_Decomposer_TR","BG_Herbivore_TR" ,
                        "BG_Predator_TR" )


abundance_indices <- c( "Plant_cover", "Grass_cover", "Legume_cover",
                        "Forb_cover", "Moss_cover" , "Phanerophyte_cover",
                        "Indigenous_cover","Archeophyte_cover","Neophyte_cover",
                        "NeophyteHerb_cover",
                        "Decomposer_Abun", "Herbivores_Abun",
                        "Predators_Abun", "Pollinators_Abun",
                        "BG_Decomposer_Abun","BG_Herbivore_Abun" ,
                        "BG_Predator_Abun" )


######  ________________SEALing  ________________#######  
# Richness along % Sealing 
SR_SealxPatch <- glm.biodiv.interaction(Biodiv_data_modif,
                                 vars = richness_indices,
                                 plots.selected = plots_all,
                                 explanatory.var = c("Seal_500", "log_Size_Patch"))

SR_Sealxage <- glm.biodiv.interaction(Biodiv_data_modif,
                                        vars = richness_indices,
                                        plots.selected = plots_all,
                                        explanatory.var = c("Seal_500", "Age"))



ggplot() +
  aes(x = Env_data$Seal_500, y = Biodiv_data_modif$Herbivores_SR,
      group = cut_number(Env_data$log_Size_Patch,3), color =cut_number(Env_data$log_Size_Patch,3)) +
  geom_point(color= "grey", alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)
                  

ggplot() +
  aes(x = Env_data$Seal_500, y = Biodiv_data_modif$Herbivores_SR,
      group = nv_data$Age, color = Env_data$Age) +
  geom_point(color= "grey", alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)


