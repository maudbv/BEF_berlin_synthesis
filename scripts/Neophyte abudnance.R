# Focus on proportion of neophytes

quartz()
Neophyte_Seal<- glm.biodiv(Biodiv_data_modif,
                                 vars = c("Neophyte_cover",
                                          "NeophyteHerb_cover",
                                          "Neophyte_SR",
                                          "Neophyte.prop",
                                          "NeophyteHerb_SR",
                                          "NeophyteHerb.prop"),
                                 plots.selected = plots_all,
                                 explanatory.var = "Seal_500")

Neophyte_N<- glm.biodiv(Biodiv_data_modif,
                           vars = c("Neophyte_cover",
                                    "NeophyteHerb_cover",
                                    "Neophyte_SR",
                                    "Neophyte.prop",
                                    "NeophyteHerb_SR",
                                    "NeophyteHerb.prop"),
                           plots.selected = plots_all,
                           explanatory.var = "N")

Neophyte_PatchSize<- glm.biodiv(Biodiv_data_modif,
                        vars = c("Neophyte_cover",
                                 "NeophyteHerb_cover",
                                 "Neophyte_SR",
                                 "Neophyte.prop",
                                 "NeophyteHerb_SR",
                                 "NeophyteHerb.prop"),
                        plots.selected = plots_all,
                        explanatory.var = "log_Size_Patch")

Neophyte_P<- glm.biodiv(Biodiv_data_modif,
                                vars = c("Neophyte_cover",
                                         "NeophyteHerb_cover",
                                         "Neophyte_SR",
                                         "Neophyte.prop",
                                         "NeophyteHerb_SR",
                                         "NeophyteHerb.prop"),
                                plots.selected = plots_all,
                                explanatory.var = "log_Road_Dist")


Neophyte_P<- glm.biodiv(Biodiv_data_modif,
                        vars = c("Neophyte_cover",
                                 "NeophyteHerb_cover",
                                 "Neophyte_SR",
                                 "Neophyte.prop",
                                 "NeophyteHerb_SR",
                                 "NeophyteHerb.prop"),
                        plots.selected = plots_all,
                        explanatory.var = "log_ShGrass_500")
