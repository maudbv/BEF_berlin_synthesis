## FIgures for presentation

quartz()
glm.biodiv(Biodiv_data_modif, vars = c("Plant_SR","Neophyte.prop",
           "Plant_cover","Neophyte_RelCover"),
                                 plots.selected = plots_all,
                                 explanatory.var = "Seal_500")

quartz()
glm.biodiv(Biodiv_data_modif, vars = c( "Decomposer_SR", "Herbivores_SR",
                                        "Predators_SR", "Pollinators_SR"),
           plots.selected = plots_all,
           explanatory.var = "Seal_500")


glm.biodiv(Biodiv_data_modif, vars = c( "Decomposer_Abun", "Herbivores_Abun",
                                        "Predators_Abun", "Pollinators_Abun"),
           plots.selected = plots_all,
           explanatory.var = "Seal_500")
