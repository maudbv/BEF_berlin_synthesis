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


Neophyte_P<- glm.biodiv(Biodiv_data_modif,
                        vars = c("Neophyte_cover",
                                 "NeophyteHerb_cover",
                                 "Neophyte_SR",
                                 "Neophyte.prop",
                                 "NeophyteHerb_SR",
                                 "NeophyteHerb.prop"),
                        plots.selected = plots_all,
                        explanatory.var = "log_Road_Dist")


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
                        explanatory.var = "log_ShGrass_500")


plot(log_Road_Dist ~ Seal_500, data = Env_data_modif)
add.stats(formula = "log_Road_Dist ~ Seal_500", data = Env_data_modif, type = "lm")

plot(log_Railw_Dist ~ Seal_500, data = Env_data_modif)
add.stats(formula = "log_Railw_Dist ~ Seal_500", data = Env_data_modif, type = "lm")


plot(RdDen_500 ~ Seal_500, data = Env_data_modif)
add.stats(formula = "RdDen_500 ~ Seal_500", data = Env_data_modif, type = "lm")
