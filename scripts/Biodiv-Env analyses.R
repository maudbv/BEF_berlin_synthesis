# Biodiversity vs. Urbanisation analyses

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
quartz()
SR.Seal500_56plots <- glm.biodiv(Biodiv_data_modif,
           vars = richness_indices,
           plots.selected = plots_all,
           explanatory.var = "Seal_500")

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
quartz()
Abun.Seal500_20plots <- glm.biodiv(Biodiv_data_modif,
                                 vars = abundance_indices,
                                 plots.selected = plots_selected_20,
                                 explanatory.var = "Seal_500")

Abun.Seal500_20plots$Type ="Abundance"
Abun.Seal500_56plots$Type ="Abundance"
Abun.Seal500_20plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))
Abun.Seal500_56plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))

## Merge all types:
GLMs_Biodiv_Sealing <- rbind(SR.Seal500_56plots, SR.Seal500_20plots,Abun.Seal500_56plots, Abun.Seal500_20plots)


quartz()
par(mfrow = c(1,2), cex = 0.7, las = 2, oma = c(6, 1,1,1))
boxplot(as.numeric(Coef) ~ Type + Organism,
        data = GLMs_Biodiv_Sealing, 
        ylim = c(-0.10, 0.30))
abline( h = 0)
boxplot(as.numeric(pDev) ~  Type + Organism,
        data = GLMs_Biodiv_Sealing)


write.csv(GLMs_Biodiv_Sealing, file = "Result outputs/GLMs_Biodiv_Sealing.csv")

#_____ Biodiversity relationship with other mitigators/variables___ ####

######  ________________Patch size ________________######
# Richness along Patch size
quartz()
SR.SizePatch_56plots <- glm.biodiv(Biodiv_data_modif,
                                 vars = richness_indices,
                                 plots.selected = plots_all,
                                 explanatory.var = "log_Size_Patch")



quartz()
SR.SizePatch_20plots <- glm.biodiv(Biodiv_data_modif,
                                 vars = richness_indices,
                                 plots.selected =plots_selected_20,
                                 explanatory.var = "log_Size_Patch")


SR.SizePatch_20plots$Type ="Richness"
SR.SizePatch_56plots$Type ="Richness"
SR.SizePatch_20plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))
SR.SizePatch_56plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))

# Abundance along log Patch Size
quartz()
Abun.SizePatch_56plots <- glm.biodiv(Biodiv_data_modif,
                                     vars = abundance_indices,
                                     plots.selected = plots_all,
                                     explanatory.var = "log_Size_Patch")
quartz()
Abun.SizePatch_20plots <- glm.biodiv(Biodiv_data_modif,
                                     vars = abundance_indices,
                                     plots.selected = plots_selected_20,
                                     explanatory.var = "log_Size_Patch")

Abun.SizePatch_20plots$Type ="Abundance"
Abun.SizePatch_56plots$Type ="Abundance"
Abun.SizePatch_20plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))
Abun.SizePatch_56plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))



######  ________________Share of herbaceous vegetation in Buffer________________######
# Richness along Herbaceous surroundings
quartz()
SR.ShHerb_56plots <- glm.biodiv(Biodiv_data_modif,
                                   vars = richness_indices,
                                   plots.selected = plots_all,
                                   explanatory.var = "ShHerb_500")

quartz()
SR.ShHerb_20plots <- glm.biodiv(Biodiv_data_modif,
                                   vars = richness_indices,
                                   plots.selected =plots_selected_20,
                                   explanatory.var = "ShHerb_500")

SR.ShHerb_20plots$Type ="Richness"
SR.ShHerb_56plots$Type ="Richness"
SR.ShHerb_20plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))
SR.ShHerb_56plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))

# Abundance along Herbaceous surroundings
quartz()
Abun.ShHerb_56plots <- glm.biodiv(Biodiv_data_modif,
                                vars = abundance_indices,
                                plots.selected = plots_all,
                                   explanatory.var = "log_Size_Patch")

quartz()
Abun.ShHerb_20plots <- glm.biodiv(Biodiv_data_modif,
                                   vars = abundance_indices,
                                   plots.selected =plots_selected_20,
                                   explanatory.var = "log_Size_Patch")

Abun.ShHerb_20plots$Type ="Abundance"
Abun.ShHerb_56plots$Type ="Abundance"
Abun.ShHerb_20plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))
Abun.ShHerb_56plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))




######  ________________Share of  Forestetation in Buffer________________######
# Richness along Forest
quartz()
SR.ShForest_56plots <- glm.biodiv(Biodiv_data_modif,
                                 vars = richness_indices,
                                 plots.selected = plots_all,
                                 explanatory.var = "ShForest_500")

quartz()
SR.ShForest_500_20plots <- glm.biodiv(Biodiv_data_modif,
                                vars = richness_indices,
                                plots.selected =plots_selected_20,
                                explanatory.var = "ShForest_500")

SR.ShForest_20plots$Type ="Richness"
SR.ShForest_56plots$Type ="Richness"
SR.ShForest_20plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))
SR.ShForest_56plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))

# Abundance along Share of Forest
quartz()
SR.ShForest_56plots <- glm.biodiv(Biodiv_data_modif,
                                vars = abundance_indices,
                                plots.selected = plots_all,
                                explanatory.var = "ShForest_500")

quartz()
SR.ShForest_20plots <- glm.biodiv(Biodiv_data_modif,
                                vars = abundance_indices,
                                plots.selected =plots_selected_20,
                                explanatory.var = "ShForest_500")

Abun.ShForest_20plots$Type ="Abundance"
Abun.ShForest_56plots$Type ="Abundance"
Abun.ShForest_20plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))
Abun.ShForest_56plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))

######  ________________Share of  Nitrogen________________######
# Richness along Forest
quartz()
SR.N_56plots <- glm.biodiv(Biodiv_data_modif,
                                  vars = richness_indices,
                                  plots.selected = plots_all,
                                  explanatory.var = "N")

quartz()
SR.N_500_20plots <- glm.biodiv(Biodiv_data_modif,
                                      vars = richness_indices,
                                      plots.selected =plots_selected_20,
                                      explanatory.var = "N")

SR.N_20plots$Type ="Richness"
SR.N_56plots$Type ="Richness"
SR.N_20plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))
SR.N_56plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))

# Abundance along Nitrogen gradient
quartz()
Abun.N_56plots <- glm.biodiv(Biodiv_data_modif,
                                  vars = abundance_indices,
                                  plots.selected = plots_all,
                                  explanatory.var = "N")

quartz()
Abun.N_20plots <- glm.biodiv(Biodiv_data_modif,
                                  vars = abundance_indices,
                                  plots.selected =plots_selected_20,
                                  explanatory.var = "N")

Abun.N_20plots$Type ="Abundance"
Abun.N_56plots$Type ="Abundance"
Abun.N_20plots$Organism <- c( rep("Nlants", 10), rep("Invertebrates", 7))
Abun.N_56plots$Organism <- c( rep("Nlants", 10), rep("Invertebrates", 7))


######  ________________Share of   Phosphorus________________######
# Richness along  Phosphorus
quartz()
SR.P_56plots <- glm.biodiv(Biodiv_data_modif,
                           vars = richness_indices,
                           plots.selected = plots_all,
                           explanatory.var = "P")

quartz()
SR.P_500_20plots <- glm.biodiv(Biodiv_data_modif,
                               vars = richness_indices,
                               plots.selected =plots_selected_20,
                               explanatory.var = "P")

SR.P_20plots$Type ="Richness"
SR.P_56plots$Type ="Richness"
SR.P_20plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))
SR.P_56plots$Organism <- c( rep("Plants", 12), rep("Invertebrates", 7))

# Abundance along P Phosphorus gradient
quartz()
Abun.P_56plots <- glm.biodiv(Biodiv_data_modif,
                             vars = abundance_indices,
                             plots.selected = plots_all,
                             explanatory.var = "P")

quartz()
Abun.P_20plots <- glm.biodiv(Biodiv_data_modif,
                             vars = abundance_indices,
                             plots.selected =plots_selected_20,
                             explanatory.var = "P")

Abun.P_20plots$Type ="Abundance"
Abun.P_56plots$Type ="Abundance"
Abun.P_20plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))
Abun.P_56plots$Organism <- c( rep("Plants", 10), rep("Invertebrates", 7))


######  ________________SVF ________________######
# Richness along SVF
quartz()
SR.SVF_56plots <- glm.biodiv(Biodiv_data_modif,
                           vars = richness_indices,
                           plots.selected = plots_all,
                           explanatory.var = "SVF")

quartz()
SR.SVF_500_20plots <- glm.biodiv(Biodiv_data_modif,
                               vars = richness_indices,
                               plots.selected =plots_selected_20,
                               explanatory.var = "SVF")

SR.SVF_20plots$Type ="Richness"
SR.SVF_56plots$Type ="Richness"
SR.SVF_20plots$Organism <- c( rep("SVFlants", 12), rep("Invertebrates", 7))
SR.SVF_56plots$Organism <- c( rep("SVFlants", 12), rep("Invertebrates", 7))

# Abundance along SVFitrogen gradient
quartz()
Abun.SVF_56plots <- glm.biodiv(Biodiv_data_modif,
                             vars = abundance_indices,
                             plots.selected = plots_all,
                             explanatory.var = "SVF")

quartz()
Abun.SVF_20plots <- glm.biodiv(Biodiv_data_modif,
                             vars = abundance_indices,
                             plots.selected =plots_selected_20,
                             explanatory.var = "SVF")

Abun.SVF_20plots$Type ="Abundance"
Abun.SVF_56plots$Type ="Abundance"
Abun.SVF_20plots$Organism <- c( rep("SVFlants", 10), rep("Invertebrates", 7))
Abun.SVF_56plots$Organism <- c( rep("SVFlants", 10), rep("Invertebrates", 7))


######  ________________ Shannon biotope diversity ________________######
# Richness along BioShan_500
quartz()
SR.BioShan_500_56plots <- glm.biodiv(Biodiv_data_modif,
                             vars = richness_indices,
                             plots.selected = plots_all,
                             explanatory.var = "BioShan_500")

quartz()
SR.BioShan_500_20plots <- glm.biodiv(Biodiv_data_modif,
                                 vars = richness_indices,
                                 plots.selected =plots_selected_20,
                                 explanatory.var = "BioShan_500")

SR.BioShan_500_20plots$Type ="Richness"
SR.BioShan_500_56plots$Type ="Richness"
SR.BioShan_500_20plots$Organism <- c( rep("BioShan_500lants", 12), rep("Invertebrates", 7))
SR.BioShan_500_56plots$Organism <- c( rep("BioShan_500lants", 12), rep("Invertebrates", 7))

# Abundance along BioShan_500
quartz()
Abun.BioShan_500_56plots <- glm.biodiv(Biodiv_data_modif,
                               vars = abundance_indices,
                               plots.selected = plots_all,
                               explanatory.var = "BioShan_500")

quartz()
Abun.BioShan_500_20plots <- glm.biodiv(Biodiv_data_modif,
                               vars = abundance_indices,
                               plots.selected =plots_selected_20,
                               explanatory.var = "BioShan_500")

Abun.BioShan_500_20plots$Type ="Abundance"
Abun.BioShan_500_56plots$Type ="Abundance"
Abun.BioShan_500_20plots$Organism <- c( rep("BioShan_500lants", 10), rep("Invertebrates", 7))
Abun.BioShan_500_56plots$Organism <- c( rep("BioShan_500lants", 10), rep("Invertebrates", 7))


######  ________________ Shannon biotope diversity ________________######
# Richness along mean_tempNight_summer
quartz()
SR.tempNight_56plots <- glm.biodiv(Biodiv_data_modif,
                                     vars = richness_indices,
                                     plots.selected = plots_all,
                                     explanatory.var = "mean_tempNight_summer")

quartz()
SR.tempNight_20plots <- glm.biodiv(Biodiv_data_modif,
                                         vars = richness_indices,
                                         plots.selected =plots_selected_20,
                                         explanatory.var = "mean_tempNight_summer")

SR.tempNight_20plots$Type ="Richness"
SR.tempNight_56plots$Type ="Richness"
SR.tempNight_20plots$Organism <- c( rep("mean_tempNight_summerlants", 12), rep("Invertebrates", 7))
SR.tempNight_56plots$Organism <- c( rep("mean_tempNight_summerlants", 12), rep("Invertebrates", 7))

# Abundance along mean_tempNight_summer
quartz()
Abun.tempNight_56plots <- glm.biodiv(Biodiv_data_modif,
                                       vars = abundance_indices,
                                       plots.selected = plots_all,
                                       explanatory.var = "mean_tempNight_summer")

quartz()
Abun.tempNight_20plots <- glm.biodiv(Biodiv_data_modif,
                                       vars = abundance_indices,
                                       plots.selected =plots_selected_20,
                                       explanatory.var = "mean_tempNight_summer")

Abun.tempNight_20plots$Type ="Abundance"
Abun.tempNight_56plots$Type ="Abundance"
Abun.tempNight_20plots$Organism <- c( rep("mean_tempNight_summerlants", 10), rep("Invertebrates", 7))
Abun.tempNight_56plots$Organism <- c( rep("mean_tempNight_summerlants", 10), rep("Invertebrates", 7))