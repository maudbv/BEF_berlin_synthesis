# EF vs. Urbanisation analyses

# necessary functions
source('scripts/functions/FUNCTION add stats.R')
source('scripts/functions/p2star.R')
source('scripts/functions/glm.biodiv.R')
source('scripts/functions/glm.biodiv.interaction.R')

# Selection of plots
plots_selected_20 <- Env_data$ID_plot[which(Env_data$Subset_20plots == 1) ]
plots_all<- Env_data$ID_plot

# Modify Env_data 
Env_data_modif <- Env_data

# outliers
Env_data_modif ["Nm_05", "ShHerb_500"]   <- NA
Env_data_modif ["Nm_09", "N"]   <- NA


# Modify EF_data 
EF_data_modif <- EF_data

# Select variables

EF_var <- c( "AGB_C", "AGB_A","AGB_N",
             "BGB_C_total", "BGB_C_fine",
             "iWUE_C","iWUE_A","iWUE_N",
             "d15N_C", "d15N_A","d15N_N",
             "decomposition_s","decomposition_k",
             "PollinationVisitCounts")

######  ________________SEALing  ________________#######  

quartz()
EF.Seal500_56plots <- glm.biodiv(EF_data_modif,
                                 vars = EF_var,
                                 plots.selected = plots_all,
                                 expl_data = Env_data_modif ,
                                 explanatory.var = "Seal_500",
                                 family.glm = "gaussian")


EF.Seal500_56plots <- glm.biodiv(EF_data_modif,
                                 vars = c( "AGB_C",  "BGB_C_fine",
                                           "iWUE_C", "d15N_C", "decomposition_k",
                                           "PollinationVisitCounts"),
                                 plots.selected = plots_all,
                                 expl_data = Env_data_modif ,
                                 explanatory.var = "Seal_500",
                                 family.glm = "gaussian")
quartz()
EF.Seal500_56plots <- glm.biodiv(EF_data_modif,
                                 vars = EF_var,
                                 plots.selected = plots_all,
                                 expl_data = Env_data_modif ,
                                 explanatory.var = "P",
                                 family.glm = "gaussian")

quartz()
EF.Seal500_56plots <- glm.biodiv(EF_data_modif,
                                 vars = EF_var,
                                 plots.selected = plots_all,
                                 expl_data = Env_data_modif ,
                                 explanatory.var = "N",
                                 family.glm = "gaussian")

quartz()
EF.Seal500_56plots <- glm.biodiv(EF_data_modif,
                                 vars = EF_var,
                                 plots.selected = plots_all,
                                 expl_data = Env_data_modif ,
                                 explanatory.var = "log_Size_Patch",
                                 family.glm = "gaussian")

quartz()
EF.Seal500_56plots <- glm.biodiv(EF_data_modif,
                                 vars = EF_var,
                                 plots.selected = plots_all,
                                 expl_data = Env_data_modif ,
                                 explanatory.var = "log_ShHerb_500",
                                 family.glm = "gaussian")
