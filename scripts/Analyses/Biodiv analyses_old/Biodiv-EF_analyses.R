## Biodiv Ecosystem functioning


# necessary functions
source('scripts/functions/FUNCTION add stats.R')
source('scripts/functions/p2star.R')
source('scripts/functions/glm.biodiv.R')
source('scripts/functions/glm.biodiv.interaction.R')

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

# Modify EF_data 
EF_data_modif <- EF_data

# Select variables
richness_indices <- c(  "Plant_SR", "Grass_SR", "Legume_SR",
                        "Forb_SR", "Moss_SR" , "Phanerophyte_SR",
                        "Indigenous_SR","Archeophyte_SR","Neophyte_SR",
                        "Neophyte.prop",
                        "NeophyteHerb_SR","NeophyteHerb.prop",
                        "Decomposer_SR", "Herbivores_SR",
                        "Predators_SR", "Pollinators_SR",
                        "BG_Decomposer_TR","BG_Herbivore_TR" ,
                        "BG_Predator_TR" )


EF_var <- c( "AGB_C", "AGB_A", "BGB_C_total", "BGB_C_fine", "iWUE_C","iWUE_A",
             "d15N_C")



######  ________________AGB  ________________#######  
# Richness along % Sealing 
quartz()
glm.biodiv(Biodiv_data_modif,  vars = richness_indices,
                                 plots.selected = plots_all,
           expl_data = EF_data_modif,
                                 explanatory.var = "AGB_C")

quartz()
SR.Seal500_20plots <- glm.biodiv(Biodiv_data_modif,
                                 vars = richness_indices,
                                 plots.selected =plots_selected_20,
                                 explanatory.var = "Seal_500")

SR.Seal500_20plots$Type ="Richness"
SR.Seal500_56plots$Type ="Richness"
SR.Seal500_20plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))
SR.Seal500_56plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))

# Abundance along % Sealing 
quartz()
Abun.Seal500_56plots <- glm.biodiv(Biodiv_data_modif,
                                   vars = abundance_indices,
                                   plots.selected = plots_all,
                                   explanatory.var = "Seal_500")