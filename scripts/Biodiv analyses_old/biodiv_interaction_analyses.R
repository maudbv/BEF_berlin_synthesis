
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


######  ________________ RICHNESS  ________________#######  
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
  geom_smooth(method = "glm", link = quasipoisson)+
  xlab('% Sealing') +
  ylab('Herbivore richness') +
  labs(colour = 'Patch Size') 

ggplot() +
  aes(x = Env_data$Seal_500, y = Biodiv_data_modif$Herbivores_SR,
      group = Env_data$Age, color = Env_data$Age) +
  geom_point(color= "grey", alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)+
  xlab('% Sealing') +
  ylab('Herbivore richness') +
  labs(colour = 'Age of plot')



######  ________________EF  ________________#######  

EF_var <- c( "AGB_C", "AGB_A","AGB_N",
             "BGB_C_total", "BGB_C_fine",
             "iWUE_C","iWUE_A","iWUE_N",
             "d15N_C", "d15N_A","d15N_N",
             "decomposition_s","decomposition_k",
             "PollinationVisitCounts")


# Biodiv * Sealing ~ EF

# signifi: Neophyte cover correlated to legume cover, with a negative interaction with sealing
# trend : Decomposition rates marginally correlated to legume cover & Sealing, with negative interaction
EFxBiodivxseal <- glm.biodiv.interaction(dat = EF_data_modif,
                                      vars = EF_var,
                                      plots.selected = plots_all,
                                      explanatory.var = c("Seal_500", "Legume_cover"),
                                      expl_data = data.frame(Env_data_modif,
                                                             Biodiv_data_modif),
                                      family.glm = "gaussian"
                                      
)


ggplot() +
  aes(x = Biodiv_data_modif$Legume_cover, y = EF_data_modif$AGB_C,
      group = cut_number(Env_data$Seal_500 ,2),
      color =cut_number(Env_data$Seal_500,2)) +
  geom_point(color= "grey", alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)+
  xlab('Legume cover') +
  ylab('Plant Aboveground Biomass') +
  labs(colour = '% Sealing') 
summary(f <-lm(EF_data_modif$AGB_C ~ Env_data$Seal_500 * Biodiv_data_modif$Legume_cover ))


##Pollination ~ PAtch size * pollinator SR

EFxBiodivxseal <- glm.biodiv.interaction(dat = EF_data_modif,
                                         vars = EF_var,
                                         plots.selected = plots_all,
                                         explanatory.var = c("Size_Patch", "Pollinators_SR"),
                                         expl_data = data.frame(Env_data_modif,
                                                                Biodiv_data_modif),
                                         family.glm = "gaussian"
                                         
)


ggplot() +
  aes(x = Biodiv_data_modif$Pollinators_SR, y = EF_data_modif$PollinationVisitCounts,
      group = cut_number( Env_data$Size_Patch,3),
      color =cut_number( Env_data$Size_Patch,3)) +
  geom_point(color= "grey", alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)+
  xlab('Pollinator Richness') +
  ylab('Pollination rate') +
  labs(colour = 'Patch size (m2)') 

tmp <- na.omit(data.frame(y = EF_data_modif[rownames(Biodiv_data_modif), ]$PollinationVisitCounts,
                  x = Biodiv_data_modif$Pollinators_SR,
                  z = Env_data$Size_Patch))
                  
                
summary(f1 <-glm(y ~ x ,tmp, family = poisson))
summary(f <-glm(y ~  x * z, tmp, family = poisson))
AIC(f1) - AIC( f) 

summary(lm(y ~  x * z, tmp))



ggplot() +
  aes(x = Biodiv_data_modif$Pollinators_SR, y = EF_data_modif$PollinationVisitCounts,
      group = cut_number( Biodiv_data_modif$NeophyteHerb_cover,2),
      color =cut_number( Biodiv_data_modif$NeophyteHerb_cover,2)) +
  geom_point( alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)+
  xlab('Pollinator Richness') +
  ylab('Pollination rate') +
  labs(colour = 'Neophyte % cover') 




summary(f1 <-glm(EF_data_modif$PollinationVisitCounts ~  Biodiv_data_modif$Pollinators_SR , family= poisson))

summary(f <-glm(EF_data_modif$PollinationVisitCounts ~  Biodiv_data_modif$Pollinators_SR * Biodiv_data_modif$NeophyteHerb_cover , family= poisson))
AIC(f1,f)
anova(f1,f)


# Signifc interaction Decomposer for Pollination rates ????
# Signif interaction Fine roots biomass and Decomposer SR
EFxBiodivxseal <- glm.biodiv.interaction(dat = EF_data_modif,
                                  vars = EF_var,
                                  plots.selected = plots_all,
                                  explanatory.var = c("Seal_500", "Decomposer_SR"),
                                  expl_data = data.frame(Env_data_modif,
                                                         Biodiv_data_modif),
                                  family.glm = "gaussian"
                                  
)

ggplot() +
  aes(x = Biodiv_data_modif$Decomposer_SR, y = EF_data_modif$	BGB_C_fine,
      group = cut_number(Env_data$Seal_500,2),
      color =cut_number(Env_data$Seal_500,2)) +
  geom_point(color= "grey", alpha = .7) +
  geom_smooth(method = "glm", link = quasipoisson)+
  xlab('Decomposer Richness') +
  ylab('Fine root biomass') +
  labs(colour = '% Sealing') 
summary(f <-lm(EF_data_modif$BGB_C_fine ~ Biodiv_data_modif$Decomposer_SR * Env_data$Seal_500 ))



EFxneoxseal <- glm.biodiv.interaction(dat = EF_data_modif,
                                      vars = EF_var,
                                      plots.selected = plots_all,
                                      explanatory.var = c("Seal_500", "BG_Decomposer_TR"),
                                      expl_data = data.frame(Env_data_modif,
                                                             Biodiv_data_modif),
                                      family.glm = "gaussian"
                                      
)
# Signifc interaction Decomposer for Pollination rates ????
EFxneoxseal <- glm.biodiv.interaction(dat = EF_data_modif,
                                      vars = EF_var,
                                      plots.selected = plots_all,
                                      explanatory.var = c("Size_Patch", "Pollinators_SR"),
                                      expl_data = data.frame(Env_data_modif,
                                                             Biodiv_data_modif),
                                      family.glm = "gaussian"
                                      
)



## Age interacts with sealing to explain pollination and decomposition K
EFxAge <- glm.biodiv.interaction(dat = EF_data_modif,
                                        vars = EF_var,
                                        plots.selected = plots_all,
                                        explanatory.var = c("Seal_500", "Age"),
                                        expl_data = data.frame(Env_data_modif,
                                                         Biodiv_data_modif),
                                    family.glm = "gaussian"
                                        
)
